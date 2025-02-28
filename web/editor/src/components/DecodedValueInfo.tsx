import React, { CSSProperties, useEffect, useRef, useState } from 'react';
import * as compiler from '../compiler';
import { DecodedValue, DecodedValueKind, TagClass, TlvPos, TlvTag } from '../wasm-definitions';
import { Box, Typography } from '@mui/material';
import { joinNodes, stringifyJSON } from '../util';

export interface DecodedValueInfoProps {
    encodedValue: string;
}

const DecodedValueInfo = ({ encodedValue }: DecodedValueInfoProps) => {
    const [values, setValues] = useState<{
        values: DecodedValue[]
    } | {
        error: string;
    }>({
        values: [],
    });

    useEffect(() => {
        compiler.derDecodeValue(encodedValue).then(res => {
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
            {values.values.map(value => getValueComponent(encodedValue, value))}
        </Box>
    );
};

export default DecodedValueInfo;

function getValueComponent(encodedValue: string, value: DecodedValue) {
    return (
        <span key={stringifyJSON(value)}>
            <HoverableText hoverElement={getTagElement(value.tag)} style={{ backgroundColor: 'lavender' }}>
                {getHexSlice(encodedValue, value.tag.pos)}
            </HoverableText>
            <HoverableText hoverElement={`Length: ${value.len.len}`} style={{ backgroundColor: 'lightblue', marginLeft: '5px' }}>
                {getHexSlice(encodedValue, value.len.pos)}
            </HoverableText>
            {value.kind.type !== 'SEQUENCE' ? (
                <HoverableText hoverElement={getValueKindElement(value.kind)} style={{ backgroundColor: 'lightgreen', marginLeft: '5px' }}>
                    {getHexSlice(encodedValue, value.valuePos)}
                </HoverableText>
            ) : (
                <Box sx={{ display: 'flex', flexDirection: 'column', margin: '10px 0px 0px 10px', gap: '10px', }}>
                    {value.kind.elements.map(element => getValueComponent(encodedValue, element))}
                </Box>
            )}
        </span>
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
    9: 'REAL',
    10: 'ENUMERATED',
    16: 'SEQUENCE',
    17: 'SET',
    18: 'NUMERIC STRING',
    19: 'PRINTABLE STRING',
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
        return (
            <>
                <span style={{ color: BRACKET_COLOR }}>[</span>
                <span style={{ color: IDENT_COLOR }}>{tag.class}</span>
                &nbsp;
                <span style={{ color: LITERAL_COLOR }}>{tag.num}</span>
                <span style={{ color: BRACKET_COLOR }}>]</span>
                {tag.class === 'UNIVERSAL' && tag.num in tagTypeMap && (
                    <>
                        &nbsp;
                        <span style={{ color: IDENT_COLOR }}>{tagTypeMap[tag.num]}</span>
                    </>
                )}
            </>
        );
    }
}

function getValueKindElement(kind: DecodedValueKind) {
    switch (kind.type) {
        case 'RAW':
            return 'Raw Data';
        case 'BOOLEAN':
            return <span style={{ color: IDENT_COLOR }}>{kind.data.toString().toUpperCase()}</span>;
        case 'INTEGER':
        case 'BIT STRING':
        case 'REAL':
            return (
                <>
                    <span style={{ color: IDENT_COLOR }}>{kind.type}</span>
                    &nbsp;
                    <span style={{ color: LITERAL_COLOR }}>{kind.data.toString()}</span>
                </>
            );
        case 'OBJECT IDENTIFIER':
            const desc = compiler.lookupOidDescription(kind.data);
            return (
                <>
                    {joinNodes(kind.data.split('.').map(node => (<span style={{ color: LITERAL_COLOR }}>{node}</span>)), (<span>.</span>))}
                    {desc && <span>&nbsp; ({desc})</span>}
                </>
            );
        case 'OCTET STRING':
            return 'Binary Data';
        case 'NULL':
            return 'NULL';
        default:
            throw new Error(`getValueKindText: ${kind.type}`);
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
                    }}>
                    {hoverElement}
                </div>
            )}
        </span>
    );
}
