import { Loc } from './asn1-ast';
export * from './asn1-ast';

export interface Request {
    type: 'custom';
    message: {
        extension: 'asn1';
        seq: number;
    } & RequestMessage;
}

export interface Response {
    type: 'custom';
    message: {
        extension: 'asn1';
        seq?: number;
    } & ResponseMessage;
}

export type RequestMessage = DiagnosticsRequest | TokenizeRequest;

export interface DiagnosticsRequest {
    type: 'diagnostics';
}

export interface TokenizeRequest {
    type: 'tokenize';
    source: string;
}

export type ResponseMessage = DiagnosticsResponse | TokenizeResponse;

export interface DiagnosticsResponse {
    type: 'diagnostics';
    errors: CompileError[];
}

export interface CompileError {
    message: string;
    path: string;
    line: number;
    col: number;
    len: number;
}

export interface TokenizeResponse {
    type: 'tokenize';
    result: Token[] | ParseError;
}

export type Token = {
    loc: Loc;
} & ({
    kind: 'valuereference';
} | {
    kind: 'typereference';
} | {
    kind: 'keyword';
    info: string;
} | {
    kind: 'number';
} | {
    kind: 'string';
    info: 'hstring' | 'bstring' | 'cstring';
} | {
    kind: 'operator';
});

export interface ParseError {
    error: string;
    loc: Loc;
}
