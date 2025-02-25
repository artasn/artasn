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

export type TaggedType = {
    tag: Tag;
} & ((QualifiedIdentifier & {
    mode: 'reference',
}) | (BuiltinType & {
    mode: 'type',
}));

export enum TagClass {
    Universal = 'UNIVERSAL',
    Application = 'APPLICATION',
    ContextSpecific = 'CONTEXT-SPECIFIC',
    Private = 'PRIVATE',
}

export interface Tag {
    class: TagClass;
    num?: number;
    kind: TagKind;
}

export enum TagKind {
    Explicit = 'EXPLICIT',
    Implicit = 'IMPLICIT',
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
}

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
}

export interface StructureComponentValue {
    name: string;
    value: ValueReference;
}

export interface ValueDefinition {
    ident: QualifiedIdentifier;
    ty: TaggedType;
    value: ValueReference;
}
