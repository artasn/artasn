import * as webfs from 'webfs';
import { getExtensionHostIFrame } from './code/VisualStudioCode';

let messageExchange: webfs.MessageExchange | null = null;

export type WebFSEventListener = (events: webfs.WebFSEvent[]) => void;
const eventListeners: WebFSEventListener[] = [];

export function addEventListener(listener: WebFSEventListener) {
    eventListeners.push(listener);
}

export function removeEventListener(listener: WebFSEventListener) {
    const index = eventListeners.indexOf(listener);
    if (index !== -1) {
        eventListeners.splice(index, 1);
    }
}

class MessageExchangeSender {
    private postMessageImpl: typeof window.postMessage | null = null;

    constructor() {
    }

    async postMessage(...args: any): Promise<void> {
        if (this.postMessageImpl === null) {
            const iframe = await getExtensionHostIFrame();
            this.postMessageImpl = iframe.contentWindow!.postMessage.bind(iframe.contentWindow);
        }

        (this.postMessageImpl as any)(...args);
    }
}

function getMessageExchange(): webfs.MessageExchange {
    if (messageExchange === null) {
        const sender = new MessageExchangeSender();
        messageExchange = {
            postMessage: sender.postMessage.bind(sender),
            addEventListener: window.addEventListener.bind(window),
        };
    }

    return messageExchange;
}

export async function getWebFS(): Promise<webfs.WebFS> {
    return await webfs.getWebFS(getMessageExchange());
}

(() => {
    window.addEventListener('message', event => {
        if (event.data) {
            const { data }: { data: webfs.EventsMessage } = event.data;
            if (data) {
                if (data.type === 'custom') {
                    const { message } = data;
                    if (message.extension === 'webfs' && message.type === 'events') {
                        for (const listener of eventListeners) {
                            listener(message.events);
                        }
                    }
                }
            }
        }
    });
})();
