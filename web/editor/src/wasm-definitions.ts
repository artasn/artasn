export interface CompileError {
    message: string;
    path: string;
    line: number;
    col: number;
    len: number;
}

export interface ModuleIdentifier {
    name: string;
    oid?: string;
}

export interface QualifiedIdentifier {
    module: ModuleIdentifier;
    name: string;
}

export type TaggedType = ((QualifiedIdentifier & {
    mode: 'reference',
    tag?: Tag;
}) | (BuiltinType & {
    mode: 'type',
    tag: Tag,
}));

export enum TagClass {
    Universal = 'UNIVERSAL',
    Application = 'APPLICATION',
    ContextSpecific = 'CONTEXT-SPECIFIC',
    Private = 'PRIVATE',
}

export interface Tag {
    class: TagClass;
    num: number;
    kind: TagKind;
    source: TagSource;
}

export enum TagKind {
    Explicit = 'EXPLICIT',
    Implicit = 'IMPLICIT',
}

export enum TagSource {
    TagImplied = 'TagImplied',
    KindImplied = 'KindImplied',
    KindSpecified = 'KindSpecified',
}

export type BuiltinType = {
    type: 'BOOLEAN';
} | {
    type: 'INTEGER';
} | {
    type: 'BIT STRING';
} | {
    type: 'OCTET STRING';
} | {
    type: 'NULL',
} | {
    type: 'OBJECT IDENTIFIER';
} | {
    type: 'REAL';
} | {
    type: 'ENUMERATED';
} | {
    type: 'SEQUENCE';
    components: StructureComponent[];
} | {
    type: 'SEQUENCE OF';
    componentType: TaggedType;
} | {
    type: 'NumericString';
} | {
    type: 'PrintableString';
};

export interface StructureComponent {
    name: string;
    componentType: TaggedType;
    defaultValue: ValueReference;
    optional: boolean;
}

export interface TypeDefinition {
    ident: QualifiedIdentifier;
    ty: TaggedType;
}

export type ValueReference = (QualifiedIdentifier & {
    mode: 'reference',
}) | (Value & {
    mode: 'value',
});

export type Value = {
    type: 'BOOLEAN';
    value: boolean;
} | {
    type: 'INTEGER';
    value: bigint;
} | {
    type: 'BIT STRING';
    // Binary-encoded bits of the string (an ASN.1 bstring).
    value: string;
} | {
    type: 'OCTET STRING';
    // Hex-encoded bytes of the octet string (an ASN.1 hstring).
    value: string;
} | {
    type: 'NULL',
    value: null;
} | {
    type: 'OBJECT IDENTIFIER';
    // X.Y.Z format of the object identifier.
    value: string;
} | {
    type: 'REAL';
    value: number;
} | {
    type: 'ENUMERATED';
} | {
    type: 'SEQUENCE';
    components: StructureComponentValue[];
} | {
    type: 'SEQUENCE OF';
    elements: ValueReference[];
} | {
    type: 'NumericString';
    value: string;
} | {
    type: 'PrintableString';
    value: string;
};

export interface StructureComponentValue {
    name: string;
    value: ValueReference;
}

export interface ValueDefinition {
    ident: QualifiedIdentifier;
    ty: TaggedType;
    value: ValueReference;
}

export interface TlvPos {
    start: number;
    end: number;
}

export interface TlvTag {
    pos: TlvPos;
    class: TagClass;
    num: number;
}

export interface TlvLen {
    pos: TlvPos;
    len: number;
}

export interface DecodedValue {
    tag: TlvTag;
    len: TlvLen;
    valuePos: TlvPos;
    kind: DecodedValueKind;
}

export type DecodedValueKind = {
    type: 'RAW';
    data: string;
} | {
    type: 'BOOLEAN';
    data: boolean;
} | {
    type: 'INTEGER';
    data: bigint;
} | {
    type: 'BIT STRING';
    data: bigint;
} | {
    type: 'OCTET STRING';
    data: string;
} | {
    type: 'NULL';
    data: null;
} | {
    type: 'OBJECT IDENTIFIER';
    data: string;
} | {
    type: 'REAL';
    data: number;
} | {
    type: 'SEQUENCE';
    elements: DecodedValue[];
} | {
    type: 'SET';
    elements: DecodedValue[];
} | {
    type: 'NumericString';
    data: string;
} | {
    type: 'PrintableString';
    data: string;
};
