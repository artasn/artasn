export interface Loc {
    offset: number;
    length: number;
}

export interface AstElement<T> {
    element: T;
    loc: Loc;
}

export interface Soi {
    ast: 'Soi';
}

export interface Eoi {
    ast: 'Eoi';
}

export interface AstTypeReference {
    ast: 'TypeReference';
    item: string;
}

export interface AstUppercaseReference {
    ast: 'UppercaseReference';
    item: string;
}

export interface AstValueReference {
    ast: 'ValueReference';
    item: string;
}

export interface AstStringLiteral {
    ast: 'StringLiteral';
    item: {
        kind: 'hstring' | 'bstring' | 'cstring';
        data: string;
    };
}

export interface AstNumber {
    ast: 'Number';
    item: bigint;
}

export interface Keyword {
    ast: 'Keyword';
    item: string;
}

export interface Operator {
    ast: 'Operator';
    item: string;
}

export interface AstBracedTokenStream {
    ast: 'BracedTokenStream';
}

export interface AstSyntaxTokenLiteral {
    ast: 'SyntaxTokenLiteral';
}
