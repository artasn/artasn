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

export type TypeReference = (QualifiedIdentifier & {
    mode: 'reference',
}) | (TaggedType & {
    mode: 'type',
});

export enum TagClass {
    Universal = 'UNIVERSAL',
    Application = 'APPLICATION',
    ContextSpecific = 'CONTEXT-SPECIFIC',
    Private = 'PRIVATE',
}

export interface Tag {
    class: TagClass;
    num?: number;
}

export enum TagKind {
    Explicit = 'EXPLICIT',
    Implicit = 'IMPLICIT',
}

export interface TaggedType {
    tag: Tag;
    kind: TagKind;
    ty: Type;
}

export type Type = {
    kind: 'BOOLEAN';
} | {
    kind: 'INTEGER';
} | {
    kind: 'BIT STRING';
} | {
    kind: 'OCTET STRING';
} | {
    kind: 'NULL',
} | {
    kind: 'OBJECT IDENTIFIER';
} | {
    kind: 'REAL';
} | {
    kind: 'ENUMERATED';
} | {
    kind: 'SEQUENCE';
    components: StructureComponent[];
}

export interface StructureComponent {
    name: string;
    componentType: TypeReference;
    defaultValue: ValueReference;
    optional: boolean;
}

export interface TypeDefinition {
    ident: QualifiedIdentifier;
    ty: TypeReference;
}

export type ValueReference = (QualifiedIdentifier & {
    mode: 'reference',
}) | (Value & {
    mode: 'value',
});

export type Value = {
    kind: 'BOOLEAN';
    value: boolean;
} | {
    kind: 'INTEGER';
    value: bigint;
} | {
    kind: 'BIT STRING';
    // Binary-encoded bits of the string (an ASN.1 bstring).
    value: string;
} | {
    kind: 'OCTET STRING';
    // Hex-encoded bytes of the octet string (an ASN.1 hstring).
    value: string;
} | {
    kind: 'NULL',
} | {
    kind: 'OBJECT IDENTIFIER';
    // X.Y.Z format of the object identifier.
    value: string;
} | {
    kind: 'REAL';
    value: number;
} | {
    kind: 'ENUMERATED';
} | {
    kind: 'SEQUENCE';
    components: StructureComponentValue[];
}

export interface StructureComponentValue {
    name: string;
    value: ValueReference;
}

export interface ValueDefinition {
    ident: QualifiedIdentifier;
    ty: TypeReference;
    value: ValueReference;
}
