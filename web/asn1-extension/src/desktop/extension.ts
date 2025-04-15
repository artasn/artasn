import { ExtensionContext } from 'vscode';
import { Request, ResponseMessage } from 'asn1-extension-protocol';
import * as libartasn from './wasm/libartasn';
import * as common from '../extension-common';
import { log } from '../log';
import { ExtensionProtocol } from '../protocol';

const protocol = new ExtensionProtocol(handleRequest);

function handleRequest(req: Request) {
    log.debug(`handleRequest: ${req.message.type}`);

    const seq = req.message.seq;

    if (req.message.type === 'tokenize') {
        sendResponse({
            type: 'tokenize',
            result: libartasn.code_tokenize(req.message.source),
        }, seq);
    } else if (req.message.type === 'diagnostics') {
        // TODO
        sendResponse({
            type: 'diagnostics',
            errors: []
        }, seq);
    } else {
        throw new Error(`not implemented: ${(req.message as any).type}`);
    }
}

function sendResponse(res: ResponseMessage, seq?: number) {
    protocol.handle({
        type: 'custom',
        message: {
            extension: 'asn1',
            seq,
            ...res,
        }
    });
}

export function activate(ctx: ExtensionContext) {
    log.info('Activated extension for desktop.');

    common.activate(ctx, protocol);
}
