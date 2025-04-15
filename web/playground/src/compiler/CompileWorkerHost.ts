import * as wasm from './wasm';

globalThis.addEventListener('message', async event => {
    const data = event.data;
    const { nonce, call } = data;
    const res = await handleCall(call, data.args ?? null);
    globalThis.postMessage({
        nonce,
        res,
    });
});

async function handleCall(call: string, args: any | null): Promise<any> {
    await wasm.ensureLoaded();
    switch (call) {
        case 'addSourceFile':
            return wasm.addSourceFile(args.path, args.source);
        case 'compile':
            return wasm.compile();
        case 'listModules':
            return wasm.listModules();
        case 'listTypes':
            return wasm.listTypes();
        case 'listValues':
            return wasm.listValues();
        case 'encodeValue':
            return wasm.encodeValue(args.transfer, args.ident);
        case 'decodeValue':
            return wasm.decodeValue(args.transfer, args.valueHex, args.options);
        case 'tokenizeCode':
            return wasm.tokenizeCode(args.source);
        default:
            return { error: `unknown call: ${call}` };
    }
}
