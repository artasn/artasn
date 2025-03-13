import React, { CSSProperties, useEffect, useRef, useState } from 'react';
import * as compiler from '../compiler';
import { DecodedValue, DecodedValueKind, DecodeOptions, isCharacterStringType, TagClass, TlvPos, TlvTag, TransferSyntax } from '../wasm-definitions';
import { Grid2 as Grid, Box, IconButton, Typography } from '@mui/material';
import { joinNodes, stringifyJSON } from '../util';
import { ChevronRight } from '@mui/icons-material';

export type DecodedValueViewMode = 'tlv' | 'components';

export interface DecodedValueInfoProps {
    encodedValue: string;
    viewMode: DecodedValueViewMode;
}

const DecodedValueInfo = ({ encodedValue, viewMode }: DecodedValueInfoProps) => {
    const [values, setValues] = useState<{
        values: DecodedValue[]
    } | {
        error: string;
    }>({
        values: [],
    });

    useEffect(() => {
        // const options = { mode: 'contextless' };
        const options: DecodeOptions = {
            mode: 'specificType',
            ident: {
                module: {
                    name: 'CycleB',
                    oid: '2.1337.1.1.66'
                },
                name: 'FileHeader',
            }
        };
        compiler.decodeValue(TransferSyntax.DER, encodedValue, options).then(res => {
            if (typeof res === 'string') {
                setValues({
                    error: res,
                });
            } else {
                setValues({
                    values: res,
                })
            }
        });
    }, [encodedValue]);

    if (encodedValue === '') {
        return (<Typography>Select a value to inspect its DER encoding</Typography>);
    }

    if ('error' in values) {
        return (<Typography>{values.error}</Typography>);
    }

    return (
        <Box sx={{ fontFamily: 'Droid Sans Mono' }}>
            {values.values.map(value =>
                <ValueComponent
                    key={stringifyJSON(value)}
                    mode={viewMode}
                    encodedValue={encodedValue}
                    value={value}
                    style={value.form.type === 'constructed'
                        ? { paddingLeft: '40px' }
                        : undefined} />
            )}
        </Box>
    );
};

export default DecodedValueInfo;

interface ValueComponentProps {
    encodedValue: string;
    mode: DecodedValueViewMode;
    value: DecodedValue;
    style?: CSSProperties;
}

const ValueComponent = ({ encodedValue, mode, value, style }: ValueComponentProps) => {
    const [expanded, setExpanded] = useState(true);

    const tagBytes = value.tag.pos.end - value.tag.pos.start;

    let rootStyle = style ?? {};
    if (value.form.type === 'constructed') {
        rootStyle = {
            ...rootStyle,
            marginLeft: '-40px',
        };
    }
    return (
        <div style={rootStyle}>
            <span style={{ display: 'inline-flex', alignItems: 'center', }}>
                {value.form.type === 'constructed' && (
                    <IconButton onClick={() => setExpanded(!expanded)}>
                        <ChevronRight sx={{
                            transform: expanded ? 'rotate(90deg)' : 'rotate(0deg)',
                        }} />
                    </IconButton>
                )}
                {mode === 'tlv' ? (
                    <>
                        <HoverableText hoverElement={getTagElement(value.tag)} style={{ backgroundColor: 'lavender' }}>
                            {getHexSlice(encodedValue, value.tag.pos)}
                        </HoverableText>
                        <HoverableText hoverElement={getLengthElement(value.len.len)} style={{ backgroundColor: 'lightblue', marginLeft: '5px' }}>
                            {getHexSlice(encodedValue, value.len.pos)}
                        </HoverableText>
                        {value.form.type == 'primitive' && (
                            <HoverableText hoverElement={getValueKindElement(value.form.kind)} style={{ backgroundColor: 'lightgreen', marginLeft: '5px' }}>
                                {getHexSlice(encodedValue, value.valuePos)}
                            </HoverableText>
                        )}
                    </>
                ) : (
                    <>
                        {value.metadata?.componentName && (
                            <span>{value.metadata.componentName}&nbsp;</span>
                        )}
                        {value.metadata?.typeIdent ? (
                            <span style={{ color: 'gray' }}>{value.metadata.typeIdent.name}</span>
                        ) : (
                            getTagElement(value.tag)
                        )}
                        &nbsp;
                        {value.form.type == 'primitive' && getValueKindElement(value.form.kind, false)}
                    </>
                )}
            </span>
            {value.form.type == 'constructed' && expanded && (
                <Grid container direction="column" sx={{ marginLeft: `${40 + tagBytes * 19 + 5}px`, gap: '10px', }}>
                    {value.form.elements.map(element => <ValueComponent key={stringifyJSON(element)} mode={mode} encodedValue={encodedValue} value={element} />)}
                </Grid>
            )}
        </div>
    );
}

function getHexSlice(encodedValue: string, pos: TlvPos): string {
    return encodedValue.slice(pos.start * 2, pos.end * 2);
}

const tagTypeMap: Record<number, string> = {
    1: 'BOOLEAN',
    2: 'INTEGER',
    3: 'BIT STRING',
    4: 'OCTET STRING',
    5: 'NULL',
    6: 'OBJECT IDENTIFIER',
    7: 'ObjectDescriptor',
    8: 'EXTERNAL',
    9: 'REAL',
    10: 'ENUMERATED',
    11: 'EMBEDDED PDV',
    12: 'UTF8String',
    13: 'RELATIVE-OID',
    14: 'TIME',
    16: 'SEQUENCE',
    17: 'SET',
    18: 'NumericString',
    19: 'PrintableString',
    20: 'TeletexString',
    21: 'VideotexString',
    22: 'IA5String',
    23: 'UTCTime',
    24: 'GeneralizedTime',
    25: 'GraphicString',
    26: 'VisibleString',
    27: 'GeneralString',
    28: 'UniversalString',
    29: 'CHARACTER STRING',
    30: 'BMPString',
    31: 'DATE',
    32: 'TIME-OF-DAY',
    33: 'DATE-TIME',
    34: 'DURATION',
};
const BRACKET_COLOR = '#0431fa';
const LITERAL_COLOR = '#098658';
const IDENT_COLOR = '#267f99';

function getTagElement(tag: TlvTag) {
    if (tag.class === TagClass.ContextSpecific) {
        return (
            <>
                <span style={{ color: BRACKET_COLOR }}>[</span>
                <span style={{ color: LITERAL_COLOR }}>{tag.num}</span>
                <span style={{ color: BRACKET_COLOR }}>]</span>
            </>
        );
    } else {
        return tag.class === 'UNIVERSAL' && tag.num in tagTypeMap ? (
            <span style={{ color: IDENT_COLOR }}>{tagTypeMap[tag.num]}</span>
        ) : (
            <>
                <span style={{ color: BRACKET_COLOR }}>[</span>
                <span style={{ color: IDENT_COLOR }}>{tag.class}</span>
                &nbsp;
                <span style={{ color: LITERAL_COLOR }}>{tag.num}</span>
                <span style={{ color: BRACKET_COLOR }}>]</span>
            </>
        );
    }
}

function getLengthElement(len: number) {
    return (
        <>
            <span>Length: </span>
            <span style={{ color: LITERAL_COLOR }}>{len}</span>
        </>
    );
}

function getValueKindElement(kind: DecodedValueKind, includeType: boolean = true) {
    switch (kind.type) {
        case 'RAW':
            if (includeType) {
                return 'Raw Data';
            } else {
                return kind.data;
            }
        case 'BOOLEAN':
            return <span style={{ color: IDENT_COLOR }}>{kind.data.toString().toUpperCase()}</span>;
        case 'INTEGER':
        case 'REAL':
            return (
                <>
                    {includeType && (
                        <>
                            <span style={{ color: IDENT_COLOR }}>{kind.type}</span>
                            &nbsp;
                        </>
                    )}
                    <span style={{ color: LITERAL_COLOR }}>{kind.data.toString()}</span>
                </>
            );
        case 'OBJECT IDENTIFIER':
            const desc = compiler.lookupOidDescription(kind.data);
            return (
                <>
                    {joinNodes(kind.data.split('.').map(node => (<span style={{ color: LITERAL_COLOR }}>{node}</span>)), (<span>.</span>))}
                    {desc && <span>&nbsp;({desc})</span>}
                </>
            );
        case 'BIT STRING':
            return <span style={{ color: LITERAL_COLOR }}>{kind.data.toString(2)}</span>;
        case 'OCTET STRING':
            if (includeType) {
                return 'Binary Data';
            } else {
                return <span style={{ color: LITERAL_COLOR }}>{kind.data}</span>;
            }
        case 'NULL':
            if (!includeType) {
                return null;
            }
            return 'NULL';
        case 'UTCTime':
            const { year, month, day, hour, minute, second, tz } = kind.data;
            const date = new Date(Date.UTC(year, month, day, hour, minute, second));
            // TODO: tz
            return date.toUTCString();
        case 'TIME':
            // TODO
            return 'TIME';
        case 'DATE':
            // TODO
            return 'DATE';
        case 'TIME-OF-DAY':
            // TODO
            return 'TIME-OF-DAY';    
        case 'DATE-TIME':
            // TODO
            return 'DATE-TIME';
        case 'DURATION':
            // TODO
            return 'DURATION';
        default:
            if (isCharacterStringType(kind.type)) {
                return (
                    <>
                        {includeType && (
                            <>
                                <span style={{ color: IDENT_COLOR }}>{kind.type}</span>
                                &nbsp;
                            </>
                        )}
                        <span style={{ color: LITERAL_COLOR }}>"{kind.data as string}"</span>
                    </>
                );
            }
    }
}

interface HoverableTextProps {
    hoverElement: React.ReactNode;
    style?: CSSProperties;
}

const HoverableText = ({ children, hoverElement, style }: React.PropsWithChildren<HoverableTextProps>) => {
    const ref = useRef<HTMLSpanElement>(null);
    const [hover, setHover] = useState({
        hovering: false,
        pos: {
            top: 0,
            left: 0,
        }
    });

    const onHover = () => {
        if (ref.current !== null) {
            const rect = ref.current.getBoundingClientRect();
            setHover({
                hovering: true,
                pos: {
                    top: rect.top + rect.height,
                    left: rect.left,
                },
            });
        }
    };

    return (
        <span
            ref={ref}
            style={style}
            onMouseEnter={onHover}
            onMouseLeave={() => setHover({ hovering: false, pos: { top: 0, left: 0, } })}>
            {children}
            {hover.hovering && (
                <div
                    style={{
                        position: 'fixed',
                        top: hover.pos.top,
                        left: hover.pos.left,
                        backgroundColor: '#f8f8f8',
                        padding: '10px',
                        border: '1px solid lightgrey',
                        borderRadius: '3px',
                        zIndex: 999,
                    }}>
                    {hoverElement}
                </div>
            )}
        </span>
    );
}
