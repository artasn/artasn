import { workspace, Disposable, FileSystemProvider, FileSearchProvider, TextSearchProvider, Uri, FileStat, FileType, FileSystemError, Event, EventEmitter, FileChangeEvent, FileSearchQuery, FileSearchOptions, TextSearchQuery, TextSearchOptions, Progress, TextSearchResult, TextSearchComplete, CancellationToken, ProviderResult, Range, Position } from 'vscode';
import { WebFS, WebFSError, WebFSErrorCode, WebFSEvent } from 'webfs';

function convertError(error: Error): FileSystemError {
    if (error instanceof WebFSError) {
        let ctr: (path: string) => FileSystemError;
        switch (error.code) {
            case WebFSErrorCode.FileExists:
                ctr = FileSystemError.FileExists;
                break;
            case WebFSErrorCode.FileNotFound:
                ctr = FileSystemError.FileNotFound;
                break;
            case WebFSErrorCode.FileNotADirectory:
                ctr = FileSystemError.FileNotADirectory;
                break;
            case WebFSErrorCode.FileIsADirectory:
                ctr = FileSystemError.FileIsADirectory;
                break;
            case WebFSErrorCode.NoPermissions:
                ctr = FileSystemError.NoPermissions;
                break;
            case WebFSErrorCode.Unavailable:
                ctr = FileSystemError.Unavailable;
                break;
            default:
                ctr = (_) => new FileSystemError(error.message);
                break;
        }
        return ctr(error.path);
    } else {
        return new FileSystemError(error.message);
    }
}

export class WebFSWrapper implements FileSystemProvider, FileSearchProvider, TextSearchProvider, Disposable {
    static scheme = 'webfs';

    private readonly disposable: Disposable;
    private readonly emitter = new EventEmitter<FileChangeEvent[]>();
    private readonly textDecoder = new TextDecoder();
    private readonly boundEventHandler: typeof this.eventHandler;

    readonly onDidChangeFile: Event<FileChangeEvent[]> = this.emitter.event;

    constructor(private fs: WebFS) {
        this.disposable = Disposable.from(
            workspace.registerFileSystemProvider(WebFSWrapper.scheme, this, { isCaseSensitive: true }),
            workspace.registerFileSearchProvider(WebFSWrapper.scheme, this),
            workspace.registerTextSearchProvider(WebFSWrapper.scheme, this)
        );
        this.boundEventHandler = this.eventHandler.bind(this);
        this.fs.addEventListener(this.boundEventHandler);
    }

    private pathToUri(path: string): Uri {
        return Uri.from({
            scheme: WebFSWrapper.scheme,
            path,
        });
    }

    private eventHandler(events: WebFSEvent[]) {
        this.emitter.fire(events.map(event => ({
            type: event.type,
            uri: this.pathToUri(event.path),
        })));
    }

    dispose() {
        this.disposable?.dispose();
        this.fs.removeEventListener(this.boundEventHandler);
    }

    stat(uri: Uri): FileStat {
        try {
            return this.fs.stat(uri.path);
        } catch (e) {
            throw convertError(e);
        }
    }

    readDirectory(uri: Uri): [string, FileType][] {
        try {
            return this.fs.readDir(uri.path);
        } catch (e) {
            throw convertError(e);
        }
    }

    async readFile(uri: Uri): Promise<Uint8Array> {
        try {
            return await this.fs.readFile(uri.path)
        } catch (e) {
            throw convertError(e);
        }
    }

    async writeFile(uri: Uri, content: Uint8Array, options: { create: boolean; overwrite: boolean; }): Promise<void> {
        try {
            await this.fs.writeFile(uri.path, content, options);
        } catch (e) {
            throw convertError(e);
        }
    }

    async rename(oldUri: Uri, newUri: Uri, options: { overwrite: boolean; }): Promise<void> {
        try {
            await this.fs.rename(oldUri.path, newUri.path, options);
        } catch (e) {
            throw convertError(e);
        }
    }

    async delete(uri: Uri): Promise<void> {
        try {
            await this.fs.delete(uri.path);
        } catch (e) {
            throw convertError(e);
        }
    }

    async createDirectory(uri: Uri): Promise<void> {
        try {
            await this.fs.mkdir(uri.path);
        } catch (e) {
            throw convertError(e);
        }
    }

    watch(_resource: Uri): Disposable {
        // ignore, fires for all changes...
        return new Disposable(() => { });
    }

    private convertSimple2RegExpPattern(pattern: string): string {
        return pattern.replace(/[\-\\\{\}\+\?\|\^\$\.\,\[\]\(\)\#\s]/g, '\\$&').replace(/[\*]/g, '.*');
    }

    // --- search provider

    provideFileSearchResults(query: FileSearchQuery, _options: FileSearchOptions, token: CancellationToken): ProviderResult<Uri[]> {
        return this.findFiles(query.pattern, token);
    }

    private findFiles(query: string | undefined, token: CancellationToken): Uri[] {
        const files = this.fs.listAll();
        const result: Uri[] = [];

        const pattern = query ? new RegExp(this.convertSimple2RegExpPattern(query)) : null;

        for (const file of files) {
            if (token.isCancellationRequested) {
                break;
            }
            if (!pattern || pattern.exec(file)) {
                result.push(this.pathToUri(file));
            }
        }

        return result;
    }

    async provideTextSearchResults(query: TextSearchQuery, options: TextSearchOptions, progress: Progress<TextSearchResult>, token: CancellationToken) {
        const result: TextSearchComplete = { limitHit: false };

        const files = this.findFiles(options.includes[0], token);
        if (files.length > 0) {
            for (const file of files) {
                if (token.isCancellationRequested) {
                    break;
                }
                const content = this.textDecoder.decode(await this.readFile(file));

                const lines = content.split('\n');
                for (let i = 0; i < lines.length; i++) {
                    const line = lines[i];
                    const index = line.indexOf(query.pattern);
                    if (index !== -1) {
                        progress.report({
                            uri: file,
                            ranges: new Range(new Position(i, index), new Position(i, index + query.pattern.length)),
                            preview: {
                                text: line,
                                matches: new Range(new Position(0, index), new Position(0, index + query.pattern.length))
                            }
                        });
                    }
                }
            }
        }

        return result;
    }
}
