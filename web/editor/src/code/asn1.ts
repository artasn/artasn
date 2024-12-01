import { CompileError } from '../wasm-definitions';
import { getExtensionHostIFrame } from './VisualStudioCode';

export interface ExtensionMessage {
    type: 'custom';
    message: {
        extension: 'asn1';
    } & ({
        type: 'diagnostics';
        errors: CompileError[];
    });
}

export async function postASN1ExtensionMessage(message: ExtensionMessage) {
    const iframe = await getExtensionHostIFrame();
    iframe.contentWindow?.postMessage(message);
}
