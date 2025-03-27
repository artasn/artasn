import React, { useEffect, useRef, useState } from 'react';
import { Button, ButtonGroup, Checkbox, Dialog, DialogActions, DialogContent, DialogTitle, FormControl, FormControlLabel, Grid2 as Grid, InputLabel, MenuItem, Select, TextField, Typography } from "@mui/material";
import DecodedValueInfo, { DecodedValueViewMode } from '../components/DecodedValueInfo';
import { FileUpload } from '@mui/icons-material';
import { toast, Bounce } from 'react-toastify';
import { TransferSyntax } from '../wasm-definitions';

enum DataFormat {
    Binary = 'binary',
    Hex = 'hex',
    Base64 = 'base64',
    PEM = 'PEM',
}

const pemBlockRegExp = new RegExp('^-----(.+?)-----$');
const textDecoder = new TextDecoder();

interface PEMBlock {
    name: string;
    data: Uint8Array;
}

function decodePEM(input: string): PEMBlock | string {
    const lines = input.split('\n');
    if (lines.length < 2) {
        return 'missing end of PEM block';
    }
    const firstLine = lines[0].trim();
    const firstLineMatch = pemBlockRegExp.exec(firstLine);
    if (firstLineMatch === null) {
        return 'malformed PEM block prefix';
    }
    const begin = firstLineMatch[1];
    if (!begin.startsWith('BEGIN ')) {
        return 'malformed PEM block prefix: expecting BEGIN';
    }
    const blockName = begin.substring('BEGIN '.length);

    const lastLine = lines[lines.length - 1].trim();
    const lastLineMatch = pemBlockRegExp.exec(lastLine);
    if (lastLineMatch === null) {
        return 'malformed PEM block suffix';
    }
    const end = lastLineMatch[1];
    if (!end.startsWith('END ')) {
        return 'malformed PEM block suffix: expecting END';
    }
    const endBlockName = end.substring('END '.length);
    if (blockName !== endBlockName) {
        return 'malformed PEM block: block name mismatch';
    }

    let base64 = '';
    for (let i = 1; i < lines.length - 1; i++) {
        const line = lines[i].trim();
        if (line === '') {
            continue;
        }
        base64 += line;
    }

    for (let i = 0; i < base64.length; i++) {
        if (!isBase64Digit(base64.charCodeAt(i))) {
            return 'malformed PEM body: illegal base64';
        }
    }

    return {
        name: blockName,
        data: Uint8Array.from(atob(base64), c => c.charCodeAt(0)),
    };
}

function decodeHex(hex: string): Uint8Array | string {
    if (hex.length % 2 != 0) {
        return 'hex string has either an extra or missing byte';
    }
    const decoded = new Uint8Array(Math.ceil(hex.length / 2));
    for (let i = 0; i < hex.length;) {
        decoded[i / 2] = parseInt(hex.slice(i, i += 2), 16);
    }
    return decoded;
}

function byteArrayEquals(a: Uint8Array, b: Uint8Array) {
    if (a.byteLength !== b.byteLength) {
        return false;
    }
    for (let i = 0; i < a.byteLength; i++) {
        if (a.at(i) !== b.at(i)) {
            return false;
        }
    }
    return true;
}

function isHexDigit(b: number) {
    const c = String.fromCharCode(b);
    return (c >= '0' && c <= '9') || (c >= 'A' && c <= 'F') || (c >= 'a' && c <= 'f');
}

function isBase64Digit(b: number) {
    const c = String.fromCharCode(b);
    return (c >= '0' && c <= '9') || (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') || c === '+' || c === '/' || c === '=';
}

function decodeData(input: Uint8Array): {
    format: DataFormat;
    data: Uint8Array;
} | string {
    const pemPrefix = Uint8Array.from('-----', c => c.charCodeAt(0));
    if (input.byteLength > 5 && byteArrayEquals(input, pemPrefix)) {
        const res = decodePEM(textDecoder.decode(input));
        if (typeof res === 'string') {
            return res;
        }
        return {
            format: DataFormat.PEM,
            data: res.data,
        };
    }

    let data: Uint8Array;
    let format: DataFormat;
    if (input.every(isHexDigit)) {
        const res = decodeHex(textDecoder.decode(input));
        if (typeof res === 'string') {
            return res;
        }
        data = res;
        format = DataFormat.Hex;
    } else if (input.every(isBase64Digit)) {
        data = Uint8Array.from(atob(textDecoder.decode(input)), c => c.charCodeAt(0));
        format = DataFormat.Base64;
    } else {
        data = input;
        format = DataFormat.Binary;
    }

    return { format, data };
}

function chunkString(str: string, chunkLength: number): string[] {
    chunkLength = Math.floor(chunkLength);
    if (chunkLength <= 0) {
        return [str];
    }

    const chunks = [];
    for (let i = 0; i < str.length; i += chunkLength) {
        chunks.push(str.substring(i, i + chunkLength));
    }
    return chunks;
}

type InputEncoding = {
    format: DataFormat;
    transferSyntax: TransferSyntax;
} | {
    error: string;
};

interface InputDialogProps {
    open: boolean;
    source: Uint8Array | null;
    encoding: InputEncoding | null;
    onClose: () => void;
    onChange: (source: string) => void;
}

const InputDialog = ({ open, source, encoding, onClose, onChange }: InputDialogProps) => {
    const isBinaryData = encoding !== null && 'format' in encoding && encoding.format === DataFormat.Binary;
    let sourceText: string | null = null;
    if (!isBinaryData) {
        if (source === null) {
            sourceText = '';
        } else {
            sourceText = textDecoder.decode(source);
        }
    }

    const onInputChanged = (source: string) => {
        if (isBinaryData) {
            onChange('');
        } else {
            onChange(source.trim());
        }
    }

    const closeModal = () => {
        onClose();
    };

    return (
        <Dialog onClose={closeModal} open={open} maxWidth="sm" fullWidth>
            <DialogTitle>Change Input</DialogTitle>
            <DialogContent>
                <Typography>Input</Typography>
                <TextField
                    slotProps={{
                        htmlInput: {
                            style: {
                                fontFamily: 'Droid Sans Mono',
                                fontSize: '10px',
                            }
                        }
                    }}
                    multiline
                    rows={10}
                    fullWidth
                    value={isBinaryData ? '[binary data]' : sourceText}
                    onChange={event => onInputChanged(event.currentTarget.value)} />
            </DialogContent>
            <DialogActions>
                <div style={{ paddingLeft: '15px' }}>
                    {encoding !== null && (
                        'error' in encoding ? (
                            <Typography variant="caption">
                                Error: {encoding.error}
                            </Typography>
                        ) : (
                            <Typography variant="caption">
                                Detected encoding: {encoding.transferSyntax}-encoded {encoding.format}
                            </Typography>
                        )
                    )}
                </div>
                <div style={{ flex: '1 0 0' }} />
                <Button onClick={closeModal}>OK</Button>
            </DialogActions>
        </Dialog>
    );
};

const DecodeView = () => {
    const fileInputRef = useRef<HTMLInputElement>(null);
    const [inputDialogOpen, setInputDialogOpen] = useState(false);
    const [input, setInput] = useState<{
        source: Uint8Array | null;
        encoding: InputEncoding | null;
    }>({
        source: null,
        encoding: null,
    });
    const [inputFileName, setInputFileName] = useState<string | null>(null);
    const [derValue, setDerValue] = useState<string | null>(null);
    const [outputTransferSyntax, setOutputTransferSyntax] = useState<TransferSyntax>(TransferSyntax.DER);
    const [parseTLV, setParseTLV] = useState(true);
    const [useDefinitions, setUseDefinitions] = useState(true);
    const [decodeViewMode, setDecodeViewMode] = useState<DecodedValueViewMode>('components');

    const [rawViewElement, setRawViewElement] = useState<HTMLDivElement | null>(null);
    const [rawViewWidth, setRawViewWidth] = useState<number | null>(null);

    useEffect(() => {
        if (rawViewElement !== null) {
            const resizeObserver = new ResizeObserver(() => {
                setRawViewWidth(rawViewElement!.clientWidth);
            });
            resizeObserver.observe(rawViewElement);
            return () => resizeObserver.disconnect();
        }
    }, [rawViewElement]);

    const onInputChanged = (source: string | Uint8Array, newInputFile?: string) => {
        if (newInputFile) {
            setInputFileName(newInputFile);
        } else if (inputFileName !== null) {
            setInputFileName(null);
            if (fileInputRef.current !== null) {
                fileInputRef.current.value = '';
            }
        }
        if (source.length === 0) {
            setInput({
                source: null,
                encoding: null,
            });
            setDerValue(null);
        } else {
            let bytes: Uint8Array;
            if (typeof source === 'string') {
                bytes = Uint8Array.from(source, c => c.charCodeAt(0));
            } else {
                bytes = source;
            }
            const res = decodeData(bytes);
            if (typeof res === 'string') {
                setInput({
                    source: bytes,
                    encoding: {
                        error: res
                    },
                });
                setDerValue(null);
            } else {
                setInput({
                    source: bytes,
                    encoding: {
                        format: res.format,
                        transferSyntax: TransferSyntax.DER,
                    },
                });

                (async () => {
                    setDerValue(Array.from(res.data)
                        .map(b => b.toString(16).toUpperCase().padStart(2, '0'))
                        .join(''));
                })();
            }
        }
    };

    const onFileInputChange = async (event: React.ChangeEvent<HTMLInputElement>) => {
        const files = event.currentTarget.files;
        if (files === null || files.length !== 1) {
            setInputFileName(null);
            setDerValue(null);
        } else {
            const file = files[0];
            const bytes = new Uint8Array(await file.arrayBuffer());
            onInputChanged(bytes, file.name);
        }
    };

    const onRawViewClicked = () => {
        if (derValue !== null) {
            const selection = window.getSelection();
            if (selection !== null) {
                if (selection.anchorNode === rawViewElement!) {
                    selection.empty();
                    return;
                }
                selection.selectAllChildren(rawViewElement!);
            }
            navigator.clipboard.writeText(derValue);

            toast.info('Copied value to clipboard!', {
                position: 'bottom-center',
                autoClose: 2000,
                hideProgressBar: true,
                closeOnClick: true,
                pauseOnHover: false,
                draggable: false,
                theme: 'colored',
                transition: Bounce,
            });
        }
    };

    return (
        <Grid container direction="column" sx={{ fontSize: '1rem', padding: '10px', height: '100%', }} spacing={2}>
            <Grid container direction="row" spacing={2} alignItems="center" sx={{ paddingRight: '10px' }}>
                <Grid container direction="column" spacing={1} alignItems="flex-end">
                    <InputDialog
                        open={inputDialogOpen}
                        source={input.source}
                        encoding={input.encoding}
                        onClose={() => setInputDialogOpen(false)}
                        onChange={onInputChanged} />
                    <ButtonGroup variant="contained">
                        <Button onClick={() => setInputDialogOpen(true)}>Change Input</Button>
                        <input
                            ref={fileInputRef}
                            style={{ display: 'none' }}
                            type="file"
                            id="decode-file-upload-button"
                            onChange={onFileInputChange} />
                        <label htmlFor="decode-file-upload-button">
                            <Button variant="contained" component="span">
                                <FileUpload />
                            </Button>
                        </label>
                    </ButtonGroup>
                    {inputFileName && <Typography variant="caption">{inputFileName}</Typography>}
                </Grid>
                <Grid flexGrow={1}>
                    <FormControl fullWidth>
                        <InputLabel id="ots-label">Output Transfer Syntax</InputLabel>
                        <Select
                            labelId="ots-label"
                            label="Output Transfer Syntax"
                            value={outputTransferSyntax}
                            onChange={event => setOutputTransferSyntax(event.target.value as TransferSyntax)}>
                            {Object.values(TransferSyntax).map(ts => (
                                <MenuItem key={ts} value={ts}>{ts}</MenuItem>
                            ))}
                        </Select>
                    </FormControl>
                </Grid>
            </Grid>
            <Grid container direction="row" spacing={0}>
                <FormControlLabel control={
                    <Checkbox
                        checked={parseTLV}
                        onChange={event => setParseTLV(event.currentTarget.checked)} />}
                    label="Parse TLV" />
                <FormControlLabel disabled={!parseTLV} control={
                    <Checkbox
                        checked={useDefinitions}
                        onChange={event => setUseDefinitions(event.currentTarget.checked)} />}
                    label="Use Definitions" />
                <FormControlLabel disabled={!parseTLV} control={
                    <Checkbox
                        checked={decodeViewMode === 'tlv'}
                        onChange={() => setDecodeViewMode(decodeViewMode === 'tlv' ? 'components' : 'tlv')} />}
                    label="Binary View" />
            </Grid>
            <Grid
                flexGrow={1}
                sx={{
                    overflow: 'scroll',
                    maxHeight: 'calc(100% - 140px)',
                    maxWidth: '100%',
                    paddingBottom: '40px',
                }}>
                {derValue && (parseTLV ? (
                    <DecodedValueInfo viewMode={decodeViewMode} encodedValue={derValue} />
                ) : (
                    <div
                        ref={ref => setRawViewElement(ref)}
                        style={{ userSelect: 'text' }}
                        onClick={onRawViewClicked}>
                        {chunkString(derValue, rawViewWidth !== null ? Math.floor(rawViewWidth / 10) : 80).map((chunk, i) => (
                            <Typography key={i.toString()} fontFamily="Droid Sans Mono">{chunk}</Typography>
                        ))}
                    </div>
                ))}
            </Grid>
        </Grid>
    );
};
export default DecodeView;
