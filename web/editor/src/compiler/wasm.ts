import init, * as lib from '../wasm/libasn1chef';
import { CompileError, DecodedValue, DecodeOptions, ModuleIdentifier, QualifiedIdentifier, TypeDefinition, ValueDefinition } from '../wasm-definitions';

let libweb: number | null = null;

type LoadCallback = () => void;
const loadCallbacks: LoadCallback[] = [];

export async function initWasm() {
    await init(fetch(`${import.meta.env.VITE_ASSET_URL}static/libasn1chef_bg.wasm`));
    libweb = lib.libweb_init();
};

function ensureInit(compiler: number | null): asserts compiler is NonNullable<number> {
    if (compiler === null) {
        throw new Error('compiler not yet initialized');
    }
}

export function addSourceFile(path: string, source: string): CompileError | null {
    ensureInit(libweb);
    return lib.compiler_add_source(libweb, path, source);
}

export function compile(): CompileError[] | null {
    ensureInit(libweb);
    return lib.compiler_compile(libweb);
}

export function listModules(): ModuleIdentifier[] {
    return lib.context_list_modules();
}

export function listTypes(): TypeDefinition[] {
    return lib.context_list_types();
}

export function listValues(): ValueDefinition[] {
    return lib.context_list_values();
}

export async function derEncodeValue(ident: QualifiedIdentifier): Promise<string> {
    ensureInit(libweb);
    return lib.context_der_encode(libweb, ident.module.name, ident.module.oid, ident.name);
}

export async function derDecodeValue(derHex: string, options: DecodeOptions): Promise<DecodedValue[] | string> {
    return lib.context_der_decode(derHex, options);
}

export async function ensureLoaded(): Promise<void> {
    if (libweb !== null) {
        return;
    }
    return new Promise(resolve => loadCallbacks.push(resolve));
}

(() => {
    if (libweb === null) {
        initWasm().then(async () => {
            for (const loadCallback of loadCallbacks) {
                loadCallback();
            }
        });
    }
})();
