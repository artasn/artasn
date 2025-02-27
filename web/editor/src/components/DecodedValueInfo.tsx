import React, { CSSProperties, useEffect, useRef, useState } from 'react';
import * as compiler from '../compiler';
import { DecodedValue, DecodedValueKind, TagClass, TlvPos, TlvTag } from '../wasm-definitions';
import { Box, Typography } from '@mui/material';
import { stringifyJSON } from '../util';

export interface DecodedValueInfoProps {
    encodedValue: string;
}

const DecodedValueInfo = ({ encodedValue }: DecodedValueInfoProps) => {
    const [values, setValues] = useState<DecodedValue[]>([]);

    useEffect(() => {
        compiler.derDecodeValue(encodedValue).then(values => setValues(values));
    }, [encodedValue]);

    if (encodedValue === '') {
        return (<Typography>Select a value to inspect its DER encoding</Typography>);
    }

    return (
        <Box sx={{ fontFamily: 'Courier New' }}>
            {values.map(value => getValueComponent(encodedValue, value))}
        </Box>
    );
};

export default DecodedValueInfo;

function getValueComponent(encodedValue: string, value: DecodedValue) {
    return (
        <span key={stringifyJSON(value)}>
            <HoverableText data={getTagText(value.tag)} style={{ backgroundColor: 'lavender' }}>
                {getHexSlice(encodedValue, value.tag.pos)}
            </HoverableText>
            <HoverableText data={`Length: ${value.len.len}`} style={{ backgroundColor: 'lightblue', marginLeft: '5px' }}>
                {getHexSlice(encodedValue, value.len.pos)}
            </HoverableText>
            {value.kind.type !== 'SEQUENCE' ? (
                <HoverableText data={getValueKindText(value.kind)} style={{ backgroundColor: 'lightgreen', marginLeft: '5px' }}>
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

function getTagText(tag: TlvTag): string {
    if (tag.class === TagClass.ContextSpecific) {
        return `[${tag.num}]`;
    } else {
        let tagText = `[${tag.class} ${tag.num}]`;
        if (tag.class === 'UNIVERSAL') {
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
            if (tag.num in tagTypeMap) {
                tagText += ` ${tagTypeMap[tag.num]}`;
            }
        }
        return tagText;
    }
}

function getValueKindText(kind: DecodedValueKind): string {
    switch (kind.type) {
        case 'RAW':
            return 'Raw Data';
        case 'BOOLEAN':
            return kind.data.toString();
        case 'INTEGER':
        case 'BIT STRING':
        case 'REAL':
            return `${kind.type} ${kind.data}`;
        case 'OBJECT IDENTIFIER':
            const desc = compiler.lookupOidDescription(kind.data);
            if (desc !== null) {
                return `${kind.data} (${desc})`;
            } else {
                return kind.data;
            }
        case 'OCTET STRING':
            return 'Binary Data';
        case 'NULL':
            return 'NULL';
        default:
            throw new Error(`getValueKindText: ${kind.type}`);
    }
}

interface HoverableTextProps {
    data: string;
    style?: CSSProperties;
}

const HoverableText = ({ children, data, style }: React.PropsWithChildren<HoverableTextProps>) => {
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
                        backgroundColor: 'lightgray',
                        padding: '10px',
                        borderRadius: '5px',
                    }}>
                    {data}
                </div>
            )}
        </span>
    );
}
