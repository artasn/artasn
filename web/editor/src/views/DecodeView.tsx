import React, { useState } from 'react';
import { Box, Checkbox, FormControl, FormControlLabel, FormGroup, Grid2 as Grid, InputLabel, MenuItem, Select, TextField, Typography } from "@mui/material";
import DecodedValueInfo from '../components/DecodedValueInfo';

enum DataFormat {
    Binary = 'binary',
    Hex = 'hex',
    Base64 = 'base64',
    PEM = 'PEM',
}

const dataFormatRegExpMap = {
    [DataFormat.Hex]: new RegExp('^[0-9A-Fa-f]+$'),
    [DataFormat.Base64]: new RegExp('^[0-9A-Za-z+/]+={0,2}$'),
};
const pemBlockRegExp = new RegExp('^-----(.+?)-----$');

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

    if (!dataFormatRegExpMap[DataFormat.Base64].test(base64)) {
        return 'malformed PEM body: illegal base64';
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

function decodeData(input: string): {
    format: DataFormat;
    data: Uint8Array;
} | string {
    if (input.startsWith('-----')) {
        const res = decodePEM(input);
        if (typeof res === 'string') {
            return res;
        }
        return {
            format: DataFormat.PEM,
            data: res.data,
        };
    }

    for (const [format, regexp] of Object.entries(dataFormatRegExpMap)) {
        if (regexp.test(input)) {
            let data: Uint8Array;
            if (format === DataFormat.Hex) {
                const res = decodeHex(input);
                if (typeof res === 'string') {
                    return res;
                }
                data = res;
            } else if (format === DataFormat.Base64) {
                data = Uint8Array.from(atob(input), c => c.charCodeAt(0));
            } else {
                throw new Error(`unsupported format: ${format}`);
            }
            return {
                format: format as DataFormat,
                data,
            };
        }
    }

    return {
        format: DataFormat.Binary,
        data: Uint8Array.from(input, c => c.charCodeAt(0)),
    };
}

enum TransferSyntax {
    BER = 'BER',
    CER = 'CER',
    DER = 'DER',
    JER = 'JER',
    XER = 'XER',
}

const DecodeView = () => {
    const [input, setInput] = useState<{
        source: string;
        encoding: {
            format: DataFormat;
            transferSyntax: TransferSyntax;
        } | {
            error: string;
        } | null;
    }>({
        source: '',
        encoding: null,
    });
    const [derValue, setDerValue] = useState<string | null>(null);
    const [outputTransferSyntax, setOutputTransferSyntax] = useState<TransferSyntax>(TransferSyntax.DER);
    const [parseTLV, setParseTLV] = useState(true);
    const [useDefinitions, setUseDefinitions] = useState(true);

    const onInputChanged = (source: string) => {
        source = source.trim();
        if (source === '') {
            setInput({
                source: '',
                encoding: null,
            });
            setDerValue(null);
        } else {
            const res = decodeData(source);
            if (typeof res === 'string') {
                setInput({
                    source,
                    encoding: {
                        error: res
                    },
                });
                setDerValue(null);
            } else {
                setInput({
                    source,
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

    return (
        <Grid container sx={{ fontSize: '1rem', padding: '10px', height: '100%' }} spacing={2}>
            <Grid size={6}>
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
                    value={input.source}
                    onChange={event => onInputChanged(event.currentTarget.value)} />
                {input.encoding && (
                    'error' in input.encoding ? (
                        <Typography variant="caption">
                            Error: {input.encoding.error}
                        </Typography>
                    ) : (
                        <Typography variant="caption">
                            Detected encoding: {input.encoding.transferSyntax}-encoded {input.encoding.format}
                        </Typography>
                    )
                )}
            </Grid>
            <Grid size={6} sx={{ height: '100%' }}>
                <Grid container spacing={2} alignItems="center">
                    <FormControl sx={{ flexGrow: 1 }}>
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
                    <Grid container direction="column" spacing={0}>
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
                    </Grid>
                </Grid>
                <Grid
                    container
                    direction="column"
                    sx={{
                        overflowY: 'scroll',
                        maxHeight: 'calc(100% - 75px)',
                        paddingBottom: '40px',
                    }}>
                    {derValue && <DecodedValueInfo encodedValue={derValue} />}
                </Grid>
            </Grid>
        </Grid>
    );
};
export default DecodeView;
