import { getExtensionHostIFrame } from './VisualStudioCode';
import type { Request, RequestMessage, ResponseMessage } from 'asn1-extension-protocol';
import { tokenizeCode } from '../compiler';

let isInit = false;

export async function initASN1Extension() {
    if (!isInit) {
        isInit = true;
        window.addEventListener('message', event => {
            const req: Request | undefined = event.data?.data;
            if (req?.type === 'custom' && req?.message.extension === 'asn1') {
                handleRequest(req.message, req.message.seq);
            }
        });
    }
}

async function handleRequest(req: RequestMessage, seq: number) {
    if (req.type === 'diagnostics') {
        // ignore diagnostics requests
        return;
    } else if (req.type === 'tokenize') {
        const result = await tokenizeCode(req.source);
        await sendASN1ExtensionResponse({
            type: 'tokenize',
            result,
        }, seq);
    }
}

export async function sendASN1ExtensionResponse(req: ResponseMessage, seq?: number) {
    const iframe = await getExtensionHostIFrame();
    iframe.contentWindow?.postMessage({
        type: 'custom',
        message: {
            extension: 'asn1',
            seq,
            ...req,
        }
    });
}
