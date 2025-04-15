import * as vscode from 'vscode';
import { log } from './log';
import { ParseError, Token, AstElement, AstProgram } from 'asn1-extension-protocol';

export type TokenizerFunc = (source: string) => Promise<Token[] | ParseError>;
export type ParseFunc = (source: string) => AstElement<AstProgram> | ParseError;

const tokenTypes = ['member', 'type', 'keyword', 'number', 'string', 'operator', 'label'];
const tokenModifiers = ['declaration', 'definition', 'member', 'builtin'];

const builtinTypeTokens = new Set([
    'ANY',
    'BOOLEAN',
    'INTEGER',
    'STRING',
    'BIT',
    'OCTET',
    'NULL',
    'OBJECT',
    'IDENTIFIER',
    'ObjectDescriptor',
    'EXTERNAL',
    'REAL',
    'ENUMERATED',
    'EMBEDDED',
    'PDV',
    'UTF8String',
    'RELATIVE-OID',
    'TIME',
    'CHOICE',
    'SEQUENCE',
    'SET',
    'OF',
    'NumericString',
    'PrintableString',
    'TeletexString',
    'VideotexString',
    'IA5String',
    'UTCTime',
    'GeneralizedTime',
    'GraphicString',
    'VisibleString',
    'GeneralString',
    'UniversalString',
    'CHARACTER',
    'BMPString',
    'DATE',
    'TIME-OF-DAY',
    'DATE-TIME',
    'DURATION',
]);

export const legend = new vscode.SemanticTokensLegend(tokenTypes, tokenModifiers);
export class Asn1SemanticTokenProvider implements vscode.DocumentSemanticTokensProvider {
    constructor(private tokenizer: TokenizerFunc, private parser: ParseFunc) { }

    provideDocumentSemanticTokens(
        document: vscode.TextDocument
    ): vscode.ProviderResult<vscode.SemanticTokens> {
        log.debug('provideDocumentSemanticTokens');
        return new Promise(async resolve => {
            const tokens = await this.tokenizer(document.getText());
            if ('error' in tokens) {
                return null;
            }

            const tokensBuilder = new vscode.SemanticTokensBuilder(legend);
            for (const token of tokens) {
                let type: string | null = null;
                let modifier: string | null = null;
                switch (token.kind) {
                    case 'valuereference':
                        // type = 'member';
                        type = 'label';
                        break;
                    case 'typereference':
                        type = 'type';
                        modifier = 'member';
                        break;
                    case 'keyword':
                        if (builtinTypeTokens.has(token.info)) {
                            type = 'type';
                            modifier = 'builtin';
                        } else {
                            type = 'keyword';
                        }
                        break;
                    case 'number':
                        type = 'number';
                        break;
                    case 'string':
                        type = 'string';
                        break;
                    case 'operator':
                        type = 'operator';
                        break;
                }
                if (type !== null) {
                    const range = new vscode.Range(document.positionAt(token.loc.offset), document.positionAt(token.loc.offset + token.loc.length));
                    if (modifier == null) {
                        tokensBuilder.push(
                            range,
                            type,
                        );
                    } else {
                        tokensBuilder.push(
                            range,
                            type,
                            [modifier],
                        );
                    }
                }
            }
            resolve(tokensBuilder.build());
        });
    }
};
