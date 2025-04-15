import { DiagnosticCollection, DiagnosticSeverity, ExtensionContext, languages, Position, Range, Uri } from 'vscode';
import { Asn1SemanticTokenProvider, legend } from './semantic-token-provider';
import { CompileError, ParseError, ResponseMessage, Token } from 'asn1-extension-protocol';
import { log } from './log';
import { ExtensionProtocol } from './protocol';

let diagnosticCollection: DiagnosticCollection;
let protocol: ExtensionProtocol;

async function tokenize(source: string): Promise<Token[] | ParseError> {
    const res = await protocol.send({
        type: 'tokenize',
        source,
    });
    if (res.type !== 'tokenize') {
        throw new Error(`expecting 'tokenize', but found '${res.type}'`);
    }
    return res.result;
}

export function activate(ctx: ExtensionContext, protocol_: ExtensionProtocol) {
    protocol = protocol_;

    languages.registerDocumentSemanticTokensProvider({
        language: 'asn1',
    }, new Asn1SemanticTokenProvider(tokenize, (_) => { throw new Error('TODO'); }), legend);

    diagnosticCollection = languages.createDiagnosticCollection('asn1');
    ctx.subscriptions.push(diagnosticCollection);

    protocol.addPushHandler(handleProtocolResponse);
}

function handleProtocolResponse(res: ResponseMessage) {
    // in the web extension, diagnostics are pushed to the extension host, instead of being requested
    if (res.type === 'diagnostics') {
        handleDiagnosticsResponse(res.errors);
    }
}

function handleDiagnosticsResponse(errors: CompileError[]) {
    diagnosticCollection.clear();

    for (const error of errors) {
        const uri = Uri.from({
            scheme: 'webfs',
            path: error.path,
        });
        const start = new Position(error.line - 1, error.col - 1);
        const end = new Position(start.line, start.character + error.len);
        diagnosticCollection.set(uri, [{
            message: error.message,
            range: new Range(start, end),
            severity: DiagnosticSeverity.Error,
        }]);
    }
}
