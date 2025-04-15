import { ExtensionContext } from 'vscode';
import { Response } from 'asn1-extension-protocol';
import * as common from '../extension-common';
import { ExtensionProtocol } from '../protocol';
import { log } from '../log';

const protocol = new ExtensionProtocol(req => postMessage(req));

export function activate(ctx: ExtensionContext) {
    log.info('Activated extension for web.');

    addEventListener('message', handleResponseMessage);

    common.activate(ctx, protocol);
}

export function deactivate() {
    removeEventListener('message', handleResponseMessage);
}

function handleResponseMessage(event: MessageEvent) {
    const res: Response | undefined = event.data;
    if (res?.type === 'custom' && res?.message.extension === 'asn1') {
        protocol.handle(res);
    }
}
