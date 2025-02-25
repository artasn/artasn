
export interface Sources {
    hash: string;
    sources: Source[];
}

export interface Source {
    hash: string;
    name: string;
    fullName: string;
    desc: string;
    registry: RegistryDescriptor;
}

export interface RegistryDescriptor {
    raw: string;
    gzip: string;
}

export type Registry = Package[];

export interface Package {
    name: string;
    modules: Module[];
    title?: string;
    url?: string;
    approval?: string;
    updates?: Package[];
    dependencies?: PackageDependency[];
}

export interface ModuleIdentifier {
    name: string;
    oid: string;
}

export type Module = ModuleIdentifier & {
    path: string;
    dependencies?: ModuleIdentifier[];
};

export interface PackageDependency {
    source: string;
    name: string;
}

export interface DownloadedPackage {
    source: string;
    name: string;
}

const rootHashKey = 'asn1chef.packageRegistry.rootHash';
const sourcesKey = 'asn1chef.packageRegistry.sources';
const downloadedKey = 'asn1chef.packageRegistry.downloaded';

export type UpdateState = {
    kind: 'checking';
} | {
    kind: 'fetching-sources';
} | {
    kind: 'fetching-registry';
    registry: string;
} | {
    kind: 'inactive';
};

export class PackageRegistry {
    constructor(private base: string) {
        if (this.base.endsWith('/')) {
            this.base = this.base.substring(0, this.base.length - 1);
        }
    }

    // Pulls all registry updates and saves them to a persistent cache.
    public async pullUpdates(callback: (state: UpdateState) => void): Promise<void> {
        callback({ kind: 'checking' });
        const currentRootHash = await this.downloadSourcesHash();

        let storedRootHash = localStorage.getItem(rootHashKey);
        if (storedRootHash !== null) {
            // root hash matches stored hash; no changes to sources
            if (storedRootHash === currentRootHash) {
                callback({ kind: 'inactive' });
                return;
            }
        }
        localStorage.setItem(rootHashKey, currentRootHash);

        callback({ kind: 'fetching-sources' });
        const { sources: currentSources } = await this.downloadSources();

        let cachedSourcesString = localStorage.getItem(sourcesKey);
        if (cachedSourcesString === null) {
            cachedSourcesString = JSON.stringify([]);
        }

        const cachedSources: Source[] = JSON.parse(cachedSourcesString);
        const updatedSources: Source[] = [];
        for (const currentSource of currentSources) {
            const cachedSource = cachedSources.find(cachedSource => cachedSource.name === currentSource.name) ?? null;
            if (cachedSource !== null) {
                if (cachedSource.hash !== currentSource.hash) {
                    updatedSources.push(currentSource);
                }
            } else {
                updatedSources.push(currentSource);
            }
        }

        localStorage.setItem(sourcesKey, JSON.stringify(currentSources));

        for (const updatedSource of updatedSources) {
            callback({ kind: 'fetching-registry', registry: updatedSource.name });
            const registry = await this.downloadRegistry(updatedSource.registry);

            localStorage.setItem(`${sourcesKey}.${updatedSource.name}`, JSON.stringify(registry));
        }

        callback({ kind: 'inactive' });
    }

    public async downloadSourcesHash(): Promise<string> {
        const res = await fetch(`${this.base}/hash.txt`);
        return await res.text();
    }

    public async downloadSources(): Promise<Sources> {
        const res = await fetch(`${this.base}/sources.json`);
        return await res.json();
    }

    public async downloadRegistry(registry: RegistryDescriptor): Promise<Registry> {
        if ('DecompressionStream' in window) {
            // download the very small gzip file and decompress it when the browser supports it
            const blob = await fetch(`${this.base}/${registry.gzip}`).then(res => res.blob());

            const gunzip = new DecompressionStream('gzip');
            const reader = blob.stream().pipeThrough(gunzip).getReader();

            const textDecoder = new TextDecoder();
            let registryText = '';
            while (true) {
                const { done, value } = await reader.read();
                if (done) {
                    break;
                }
                registryText += textDecoder.decode(value);
            }

            return JSON.parse(registryText);
        } else {
            // download the massive json file and hope that the HTTP server compresses it
            return await fetch(`${this.base}/${registry.raw}`).then(res => res.json());
        }
    }

    public getSources(): Source[] {
        const s = localStorage.getItem(sourcesKey);
        if (s === null) {
            return [];
        }
        return JSON.parse(s);
    }

    public getRegistry(sourceName: string): Registry {
        const s = localStorage.getItem(`${sourcesKey}.${sourceName}`);
        if (s === null) {
            return [];
        }
        return JSON.parse(s);
    }

    // Fetches an ASN.1 module from the package registry.
    public async downloadModule(sourceName: string, modulePath: string): Promise<ArrayBuffer> {
        const res = await fetch(`${this.base}/registry/${sourceName}/${modulePath}`);
        return await res.arrayBuffer();
    }

    public getDownloadedPackages(): DownloadedPackage[] {
        const s = localStorage.getItem(downloadedKey);
        if (s === null) {
            localStorage.setItem(downloadedKey, JSON.stringify([]));
            return [];
        }
        return JSON.parse(s);
    }

    public savePackage(sourceName: string, name: string) {
        const downloaded = this.getDownloadedPackages();
        downloaded.push({
            source: sourceName,
            name,
        });
        localStorage.setItem(downloadedKey, JSON.stringify(downloaded));
    }

    public deletePackage(sourceName: string, name: string) {
        const downloaded = this.getDownloadedPackages();
        const index = downloaded.findIndex(pkg => pkg.source === sourceName && pkg.name === name);
        if (index !== -1) {
            downloaded.splice(index, 1);
            localStorage.setItem(downloadedKey, JSON.stringify(downloaded));
        }
    }
}


const BASE_URL = import.meta.env.VITE_PACKAGE_REGISTRY_URL;
export const packageRegistry = new PackageRegistry(BASE_URL);
