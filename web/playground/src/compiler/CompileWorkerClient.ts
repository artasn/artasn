import { CompileError, DecodedValue, DecodeOptions, ModuleIdentifier, QualifiedIdentifier, TransferSyntax, TypeDefinition, ValueDefinition } from '../wasm-definitions';

type WorkerCallback = (res: any) => void;

const worker = new Worker(new URL('./CompileWorkerHost.ts', import.meta.url), {
    type: 'module',
});
const callbacks = new Map<number, WorkerCallback>();
let callNonce = 0;

(() => {
    worker.addEventListener('message', event => {
        const data = event.data;
        const callback = callbacks.get(data.nonce);
        if (callback) {
            callback(data.res);
        }
    });


})();

async function callWorker(call: string, args?: any): Promise<any> {
    return new Promise(resolve => {
        const nonce = callNonce++;
        callbacks.set(nonce, resolve);

        worker.postMessage({
            nonce,
            call,
            args,
        });
    });
}

export async function addSourceFile(path: string, source: string): Promise<CompileError | null> {
    return await callWorker('addSourceFile', { path, source });
}

export async function compile(): Promise<CompileError[] | null> {
    return await callWorker('compile');
}

export async function listModules(): Promise<ModuleIdentifier[]> {
    return await callWorker('listModules');
}

export async function listTypes(): Promise<TypeDefinition[]> {
    return await callWorker('listTypes');
}

export async function listValues(): Promise<ValueDefinition[]> {
    return await callWorker('listValues');
}

export async function encodeValue(transfer: TransferSyntax, ident: QualifiedIdentifier): Promise<string> {
    return await callWorker('encodeValue', { transfer, ident });
}

export async function decodeValue(transfer: TransferSyntax, valueHex: string, options: DecodeOptions): Promise<DecodedValue[] | string> {
    return await callWorker('decodeValue', { transfer, valueHex, options });
}
