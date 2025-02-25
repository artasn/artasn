import { ConfigurationTarget, DiagnosticCollection, DiagnosticSeverity, ExtensionContext, languages, Position, Range, Uri, workspace, WorkspaceConfiguration } from 'vscode';

let diagnosticCollection: DiagnosticCollection;

interface CompileError {
    message: string;
    path: string;
    line: number;
    col: number;
    len: number;
}

interface ExtensionMessage {
    type: 'custom';
    message: {
        extension: 'asn1';
    } & ({
        type: 'diagnostics';
        errors: CompileError[];
    });
}


export function activate(ctx: ExtensionContext) {
    diagnosticCollection = languages.createDiagnosticCollection('asn1');
    ctx.subscriptions.push(diagnosticCollection);

    addEventListener('message', handleMessage);
}

export function deactivate() {
    removeEventListener('message', handleMessage);
}

function handleMessage(event: MessageEvent) {
    const data: ExtensionMessage | undefined = event.data;
    if (data?.type === 'custom') {
        const message = data.message;
        if (message?.extension === 'asn1') {
            if (message.type === 'diagnostics') {
                diagnosticCollection.clear();
                for (const error of message.errors) {
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
        }
    }
}
