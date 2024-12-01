// --- vs types

/**
 * Enumeration of file change types.
 */
export enum FileChangeType {

	/**
	 * The contents or metadata of a file have changed.
	 */
	Changed = 1,

	/**
	 * A file has been created.
	 */
	Created = 2,

	/**
	 * A file has been deleted.
	 */
	Deleted = 3,
}

/**
 * Permissions of a file.
 */
export enum FilePermission {
	/**
	 * The file is readonly.
	 *
	 * *Note:* All `FileStat` from a `FileSystemProvider` that is registered with
	 * the option `isReadonly: true` will be implicitly handled as if `FilePermission.Readonly`
	 * is set. As a consequence, it is not possible to have a readonly file system provider
	 * registered where some `FileStat` are not readonly.
	 */
	Readonly = 1
}

/**
 * The `FileStat`-type represents metadata about a file
 */
export interface FileStat {
	/**
	 * The type of the file, e.g. is a regular file, a directory, or symbolic link
	 * to a file.
	 *
	 * *Note:* This value might be a bitmask, e.g. `FileType.File | FileType.SymbolicLink`.
	 */
	type: FileType;
	/**
	 * The creation timestamp in milliseconds elapsed since January 1, 1970 00:00:00 UTC.
	 */
	ctime: number;
	/**
	 * The modification timestamp in milliseconds elapsed since January 1, 1970 00:00:00 UTC.
	 *
	 * *Note:* If the file changed, it is important to provide an updated `mtime` that advanced
	 * from the previous value. Otherwise there may be optimizations in place that will not show
	 * the updated file contents in an editor for example.
	 */
	mtime: number;
	/**
	 * The size in bytes.
	 *
	 * *Note:* If the file changed, it is important to provide an updated `size`. Otherwise there
	 * may be optimizations in place that will not show the updated file contents in an editor for
	 * example.
	 */
	size: number;
	/**
	 * The permissions of the file, e.g. whether the file is readonly.
	 *
	 * *Note:* This value might be a bitmask, e.g. `FilePermission.Readonly | FilePermission.Other`.
	 */
	permissions?: FilePermission;
}

/**
 * Enumeration of file types. The types `File` and `Directory` can also be
 * a symbolic links, in that case use `FileType.File | FileType.SymbolicLink` and
 * `FileType.Directory | FileType.SymbolicLink`.
 */
export enum FileType {
	/**
	 * The file type is unknown.
	 */
	Unknown = 0,
	/**
	 * A regular file.
	 */
	File = 1,
	/**
	 * A directory.
	 */
	Directory = 2,
	/**
	 * A symbolic link to a file.
	 */
	SymbolicLink = 64
}

// --- webfs types

// Adapted from `{ FileSystemProviderErrorCode } from 'vs/platform/files/common/files'`
export enum WebFSErrorCode {
	FileExists = 'EntryExists',
	FileNotFound = 'EntryNotFound',
	FileNotADirectory = 'EntryNotADirectory',
	FileIsADirectory = 'EntryIsADirectory',
	FileExceedsStorageQuota = 'EntryExceedsStorageQuota',
	FileTooLarge = 'EntryTooLarge',
	FileWriteLocked = 'EntryWriteLocked',
	NoPermissions = 'NoPermissions',
	Unavailable = 'Unavailable',
	Unknown = 'Unknown'
}

export interface File extends FileStat {
	id: string;
	name: string;
}

function createFile(name: string): File {
	return {
		type: FileType.File,
		id: generateID(),
		name,
		ctime: Date.now(),
		mtime: Date.now(),
		size: 0,
	};
}

export interface Directory extends FileStat {
	id: string;
	name: string;
	// IDs of each entry in the directory.
	children: string[];
}

function createDirectory(name: string, id: string = generateID()): Directory {
	return {
		type: FileType.Directory,
		id,
		name,
		ctime: Date.now(),
		mtime: Date.now(),
		size: 0,
		children: [],
	};
}

export type Entry = File | Directory;

export class WebFSError extends Error {
	constructor(public path: string, public code: WebFSErrorCode) {
		super(`${code}: ${path}`);
	}
}

class Database {
	private static DB_NAME = 'webfs';
	private static ENTRY_TABLE = 'entries';
	private static DATA_TABLE = 'data';
	private static VERSION = 3;

	private db: IDBDatabase = null as unknown as IDBDatabase;

	async init(): Promise<void> {
		this.db = await this.openDB();
		if (await this.getEntry('root') === null) {
			await this.updateEntry(createDirectory('', 'root'));
		}
	}

	private async createObjectStore(db: IDBDatabase, name: string, options?: IDBObjectStoreParameters): Promise<void> {
		return new Promise((resolve, reject) => {
			const store = db.createObjectStore(name, options);
			store.transaction.onerror = () => reject(store.transaction.error);
			store.transaction.oncomplete = () => resolve();
		});
	}

	private async upgradeDB(db: IDBDatabase, version: number): Promise<IDBDatabase> {
		return new Promise(async resolve => {

			if (version === 1) {
				await this.createObjectStore(db, Database.ENTRY_TABLE, {
					keyPath: 'id',
				});
			} else if (version === 2) {
				await this.createObjectStore(db, Database.DATA_TABLE);
			} else {
				return db;
			}

			db.close();

			const request = globalThis.indexedDB.open(Database.DB_NAME, version + 1);
			request.onupgradeneeded = async (event) => {
				resolve(await this.upgradeDB(request.result, event.oldVersion + 1));
			};
			request.onsuccess = () => {
				if (request.result.version === Database.VERSION) {
					resolve(request.result);
				}
			}
		});
	}

	private async openDB(): Promise<IDBDatabase> {
		return new Promise(resolve => {
			const request = globalThis.indexedDB.open(Database.DB_NAME);
			request.onupgradeneeded = async (event) => {
				resolve(await this.upgradeDB(request.result, event.oldVersion + 1));
			};
			request.onsuccess = () => {
				if (request.result.version === Database.VERSION) {
					resolve(request.result);
				}
			};
		});
	}

	public async getEntries(): Promise<Entry[]> {
		return new Promise((resolve, reject) => {
			const transaction = this.db.transaction(Database.ENTRY_TABLE, 'readonly');
			const store = transaction.objectStore(Database.ENTRY_TABLE);
			const request = store.getAll();
			request.onerror = () => reject(request.error);
			request.onsuccess = () => resolve(request.result as Entry[]);
		});
	}

	public async getEntry(id: string): Promise<Entry | null> {
		return await this.getObject(Database.ENTRY_TABLE, id) ?? null;
	}

	public async getData(id: string): Promise<Uint8Array | null> {
		const buffer = await this.getObject(Database.DATA_TABLE, id) ?? null;
		if (buffer !== null) {
			return new Uint8Array(buffer as ArrayBuffer);
		}
		return null;
	}

	private async getObject(storeName: string, id: string): Promise<any> {
		return new Promise((resolve, reject) => {
			const transaction = this.db.transaction(storeName, 'readonly');
			const store = transaction.objectStore(storeName);
			const request = store.get(id);
			request.onerror = () => reject(request.error);
			request.onsuccess = () => resolve(request.result);
		});
	}

	public async updateEntry(entry: Entry): Promise<void> {
		await this.updateObject(Database.ENTRY_TABLE, entry);
	}

	public async updateData(id: string, data: Uint8Array): Promise<void> {
		await this.updateObject(Database.DATA_TABLE, data.buffer.slice(data.byteOffset, data.byteOffset + data.byteLength), id);
	}

	private async updateObject(storeName: string, data: any, id?: IDBValidKey): Promise<void> {
		return new Promise((resolve, reject) => {
			const transaction = this.db.transaction(storeName, 'readwrite');
			const store = transaction.objectStore(storeName);
			const result = store.put(data, id);
			result.onerror = () => {
				transaction.abort();
				reject(result.error);
			};
			result.onsuccess = async () => {
				transaction.commit();
				resolve();
			};
		});
	}

	public async deleteEntry(id: string): Promise<void> {
		await this.deleteObject(Database.ENTRY_TABLE, id);
	}

	public async deleteData(id: string): Promise<void> {
		await this.deleteObject(Database.DATA_TABLE, id);
	}

	private async deleteObject(storeName: string, id: IDBValidKey): Promise<void> {
		return new Promise((resolve, reject) => {
			const transaction = this.db.transaction(storeName, 'readwrite');
			const store = transaction.objectStore(storeName);
			const result = store.delete(id);
			result.onerror = () => {
				transaction.abort();
				reject(result.error);
			};
			result.onsuccess = async () => {
				transaction.commit();
				resolve();
			};
		});
	}
}

export interface WebFSEvent {
	type: FileChangeType;
	path: string;
}

export type WebFSEventListener = (events: WebFSEvent[]) => void;

export interface MessageExchange {
	postMessage: typeof postMessage;
	addEventListener: typeof addEventListener;
}

export interface EventsMessage {
	type: 'custom';
	message: {
		extension: 'webfs';
		type: 'events';
		events: WebFSEvent[];
	};
}

type LoadCallback = () => void;

export class WebFS {
	private db = new Database();
	private root = null as unknown as Directory;
	private entries = new Map<String, Entry>();
	private loading = false;
	private loadCallbacks: LoadCallback[] = [];

	private bufferedEvents: {
		source: 'local' | 'foreign';
		event: WebFSEvent;
	}[] = [];
	private fireSoonHandle: any | null = null;
	private eventListeners: WebFSEventListener[] = [];

	constructor(private messageExchange: MessageExchange) {
		this.messageExchange.addEventListener('message', this.onMessage.bind(this));
	}

	async init(): Promise<void> {
		if (this.root) {
			return;
		}
		if (this.loading) {
			return new Promise(resolve => this.loadCallbacks.push(resolve));
		}

		this.loading = true;
		await this.db.init();

		const entries = await this.db.getEntries();
		for (const entry of entries) {
			this.entries.set(entry.id, entry);
		}
		this.root = this.getEntryByID('root') as Directory;
		this.loading = false;

		for (const loadCallback of this.loadCallbacks) {
			loadCallback();
		}
	}

	async refresh(): Promise<void> {
		const entries = await this.db.getEntries();
		this.entries.clear();
		for (const entry of entries) {
			this.entries.set(entry.id, entry);
		}
	}

	private async onMessage(event: MessageEvent) {
		const data: EventsMessage | undefined = event.data?.data;
		if (data?.type === 'custom') {
			if (data.message.extension === 'webfs' && data.message.type === 'events') {
				this.fireSoon(data.message.events, 'foreign');
				let refreshNeeded = false;
				for (const event of data.message.events) {
					if (event.type === FileChangeType.Created || event.type === FileChangeType.Deleted) {
						refreshNeeded = true;
						break;
					}
				}
				if (refreshNeeded) {
					await this.refresh();
				}
			}
		}
	}

	public async mkdir(path: string): Promise<void> {
		const basename = this.basename(path);
		const dirname = this.dirname(path);
		const parent = this.getEntryByPath(dirname);
		if (parent.type !== FileType.Directory) {
			throw new WebFSError(dirname, WebFSErrorCode.FileNotADirectory);
		}
		const parentDir = parent as Directory;
		const existingEntry = parentDir.children.find(id => this.getEntryByID(id)!.name === basename) ?? null;
		if (existingEntry !== null) {
			throw new WebFSError(path, WebFSErrorCode.FileExists);
		}

		const dir = createDirectory(basename);
		this.entries.set(dir.id, dir);
		await this.db.updateEntry(dir);

		parentDir.children.push(dir.id);
		parentDir.mtime = Date.now();
		parentDir.size++;
		await this.db.updateEntry(parentDir);

		this.fireSoon([{ type: FileChangeType.Created, path }, { type: FileChangeType.Changed, path: dirname }]);
	}

	public stat(path: string): FileStat {
		return this.getEntryByPath(path);
	}

	public readDir(path: string): [string, FileType][] {
		const entry = this.getEntryByPath(path);
		if (entry.type === FileType.Directory) {
			const dir = entry as Directory;
			return dir.children.map(id => {
				const entry = this.getEntryByID(id)!;
				return [entry.name, entry.type];
			});
		} else {
			throw new WebFSError(path, WebFSErrorCode.FileNotADirectory);
		}
	}

	public async readFile(path: string): Promise<Uint8Array> {
		const entry = this.getEntryByPath(path);
		if (entry.type === FileType.Directory) {
			throw new WebFSError(path, WebFSErrorCode.FileIsADirectory);
		}
		return (await this.db.getData(entry.id))!;
	}

	public async writeFile(path: string, data: Uint8Array, options: { create: boolean; overwrite: boolean; }): Promise<void> {
		const basename = this.basename(path);
		const dirname = this.dirname(path);
		const parent = this.getEntryByPath(dirname);
		if (parent.type !== FileType.Directory) {
			throw new WebFSError(path, WebFSErrorCode.FileNotADirectory);
		}
		const parentDir = parent as Directory;
		let fileEntry = parentDir.children.map(id => this.getEntryByID(id)!).find(entry => entry.name === basename) ?? null;
		if (fileEntry !== null) {
			if (fileEntry.type === FileType.Directory) {
				throw new WebFSError(path, WebFSErrorCode.FileIsADirectory);
			}
			if (!options.overwrite) {
				throw new WebFSError(path, WebFSErrorCode.FileExists);
			}
		} else {
			if (!options.create) {
				throw new WebFSError(path, WebFSErrorCode.FileNotFound);
			}

			fileEntry = createFile(basename);
			this.entries.set(fileEntry.id, fileEntry);
			await this.db.updateEntry(fileEntry);

			parentDir.children.push(fileEntry.id);
			parentDir.mtime = Date.now();
			parentDir.size++;
			await this.db.updateEntry(parentDir);

			this.fireSoon([{ type: FileChangeType.Created, path }, { type: FileChangeType.Changed, path: dirname }]);
		}

		await this.db.updateData(fileEntry.id, data);

		fileEntry.mtime = Date.now();
		fileEntry.size = data.byteLength;
		await this.db.updateEntry(fileEntry);

		this.fireSoon([{ type: FileChangeType.Changed, path }]);
	}

	public async rename(oldPath: string, newPath: string, options: { overwrite: boolean; }): Promise<void> {
		const oldEntry = this.getEntryByPath(oldPath);

		const oldDirname = this.dirname(oldPath);
		const newDirname = this.dirname(newPath);
		const oldParent = this.getEntryByPath(oldDirname);
		const newParent = this.getEntryByPath(newDirname);

		if (oldParent.type !== FileType.Directory) {
			throw new WebFSError(oldPath, WebFSErrorCode.FileNotADirectory);
		}
		if (newParent.type !== FileType.Directory) {
			throw new WebFSError(newPath, WebFSErrorCode.FileNotADirectory);
		}

		const oldParentDir = oldParent as Directory;
		const newParentDir = newParent as Directory;

		const newBasename = this.basename(newPath);
		const existingNewEntry = newParentDir.children.map(id => this.getEntryByID(id)!).find(entry => entry.name === newBasename) ?? null;
		if (existingNewEntry !== null && !options.overwrite) {
			throw new WebFSError(newPath, WebFSErrorCode.FileExists);
		}

		oldParentDir.children.splice(oldParentDir.children.indexOf(oldEntry.id), 1);
		oldParentDir.size--;
		await this.db.updateEntry(oldParentDir);

		oldEntry.name = newBasename;

		this.fireSoon([{ type: FileChangeType.Deleted, path: oldPath }]);

		if (existingNewEntry !== null) {
			this.entries.delete(existingNewEntry.id);
			newParentDir.children.splice(newParentDir.children.indexOf(existingNewEntry.id), 1);
			newParentDir.children.push(oldEntry.id);
			await this.db.updateEntry(newParentDir);

			this.fireSoon([{ type: FileChangeType.Changed, path: newPath }]);
		} else {
			newParentDir.children.push(oldEntry.id);
			newParentDir.mtime = Date.now();
			newParentDir.size--;
			await this.db.updateEntry(newParentDir);

			this.fireSoon([{ type: FileChangeType.Created, path: newPath }, { type: FileChangeType.Changed, path: newDirname }]);
		}
	}

	public async delete(path: string): Promise<void> {
		if (path === '/') {
			// prevent deleting the root dir
			throw new WebFSError(path, WebFSErrorCode.NoPermissions);
		}

		const basename = this.basename(path);
		const dirname = this.dirname(path);
		const parent = this.getEntryByPath(dirname);
		if (parent.type !== FileType.Directory) {
			throw new WebFSError(path, WebFSErrorCode.FileNotADirectory);
		}
		const parentDir = parent as Directory;
		const existingEntry = parentDir.children.map(id => this.getEntryByID(id)!).find(entry => entry.name === basename) ?? null;
		if (existingEntry === null) {
			throw new WebFSError(path, WebFSErrorCode.FileNotFound);
		}

		// delete the entry and its children recursively
		await this.deleteByID(existingEntry.id);

		parentDir.children.splice(parentDir.children.indexOf(existingEntry.id), 1);
		parentDir.mtime = Date.now();
		parentDir.size--;
		await this.db.updateEntry(parentDir);

		this.fireSoon([{ type: FileChangeType.Deleted, path }, { type: FileChangeType.Changed, path: dirname }]);
	}

	private async deleteByID(id: string): Promise<void> {
		const entry = this.getEntryByID(id)!;
		if (entry.type === FileType.Directory) {
			const entryDir = entry as Directory;
			for (const child of entryDir.children) {
				await this.deleteByID(child);
			}
		} else {
			await this.db.deleteData(id);
		}

		this.entries.delete(id);
		await this.db.deleteEntry(id);
	}

	public basename(path: string): string {
		path = rtrim(path, '/');
		if (path.length === 0) {
			return '';
		}

		return path.substring(path.lastIndexOf('/') + 1);
	}

	public dirname(path: string): string {
		path = rtrim(path, '/');
		if (path.length === 0) {
			return '/';
		}

		return path.substring(0, path.lastIndexOf('/')) || '/';
	}

	public listAll(): string[] {
		const list: string[] = [];
		list.push('/');
		this.listAllChildren(list, '', this.root);
		return list;
	}

	public exists(path: string): boolean {
		try {
			this.stat(path);
			return true;
		} catch {
			return false;
		}
	}

	private listAllChildren(list: string[], dirname: string, dir: Directory) {
		for (const child of dir.children.map(id => this.getEntryByID(id)!)) {
			const fullPath = dirname + '/' + child.name;
			list.push(fullPath);
			if (child.type === FileType.Directory) {
				this.listAllChildren(list, fullPath, child as Directory);
			}
		}
	}

	public addEventListener(listener: WebFSEventListener) {
		this.eventListeners.push(listener);
	}

	public removeEventListener(listener: WebFSEventListener) {
		const index = this.eventListeners.indexOf(listener);
		if (index !== -1) {
			this.eventListeners.splice(index, 1);
		}
	}

	private getEntryByPath(path: string): Entry {
		if (!path.startsWith('/')) {
			throw new WebFSError(path, WebFSErrorCode.FileNotFound);
		}
		if (path === '/') {
			return this.root;
		}
		const segments = path.substring(1).split('/');
		let currentEntry = this.root;
		segmentLoop:
		for (const [index, segment] of segments.entries()) {
			for (const childID of currentEntry.children) {
				const child = this.getEntryByID(childID)!;
				if (child.name === segment) {
					if (child.type === FileType.Directory) {
						currentEntry = child as Directory;
						continue segmentLoop;
					} else {
						if (index !== segments.length - 1) {
							throw new WebFSError(path, WebFSErrorCode.FileNotADirectory);
						}
						// the basename of the path is a file; return it
						return child;
					}
				}
			}
			throw new WebFSError(path, WebFSErrorCode.FileNotFound);
		}
		// the basename of the path is a directory; return it
		return currentEntry;
	}

	private getEntryByID(id: string): Entry | null {
		return this.entries.get(id) ?? null;
	}

	private fireSoon(events: WebFSEvent[], source: 'local' | 'foreign' = 'local') {
		this.bufferedEvents.push(...events.map(event => ({
			source,
			event,
		})));

		if (this.fireSoonHandle) {
			clearTimeout(this.fireSoonHandle);
		}

		this.fireSoonHandle = setTimeout(() => {
			const sourcedEvents = [...this.bufferedEvents];
			this.bufferedEvents = [];
			for (const listener of this.eventListeners) {
				listener(sourcedEvents.map(sourcedEvent => sourcedEvent.event));
			}

			const localEvents = sourcedEvents.filter(sourcedEvent => sourcedEvent.source === 'local').map(sourcedEvent => sourcedEvent.event);
			if (localEvents.length > 0) {
				const message: EventsMessage = {
					type: 'custom',
					message: {
						extension: 'webfs',
						type: 'events',
						events: localEvents,
					}
				};
				this.messageExchange.postMessage(message);
			}
		}, 5);
	}
}

function rtrim(haystack: string, needle: string): string {
	if (!haystack || !needle) {
		return haystack;
	}

	const needleLen = needle.length,
		haystackLen = haystack.length;

	if (needleLen === 0 || haystackLen === 0) {
		return haystack;
	}

	let offset = haystackLen,
		idx = -1;

	while (true) {
		idx = haystack.lastIndexOf(needle, offset - 1);
		if (idx === -1 || idx + needleLen !== offset) {
			break;
		}
		if (idx === 0) {
			return '';
		}
		offset = idx;
	}

	return haystack.substring(0, offset);
}

// Generate a 16-byte hex-encoded unique identifier.
function generateID(): string {
	const array = new Uint8Array(16);
	crypto.getRandomValues(array);
	return Array.from(array)
		.map((i) => i.toString(16).padStart(2, '0'))
		.join('');
}

declare const globalThis: {
	indexedDB: IDBFactory;
	webfs?: WebFS;
};

export async function getWebFS(messageExchange: MessageExchange): Promise<WebFS> {
	if (!globalThis.webfs) {
		globalThis.webfs = new WebFS(messageExchange);
	}
	const webfs = globalThis.webfs;
	await webfs.init();
	return webfs;
}
