import * as vscode from 'vscode';
import { WebFSWrapper } from './webfs-wrapper';
import { getWebFS } from 'webfs';

declare const navigator: unknown;

export async function activate(context: vscode.ExtensionContext) {
	if (typeof navigator === 'object') { // do not run under node.js
		await enableFS(context);
	}
}

async function enableFS(context: vscode.ExtensionContext) {
	const webFS = await getWebFS({
		addEventListener: globalThis.addEventListener.bind(globalThis),
		postMessage: globalThis.postMessage.bind(globalThis),
	});
	const wrapper = new WebFSWrapper(webFS);
	context.subscriptions.push(wrapper);
}
