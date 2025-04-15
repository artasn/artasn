import type { Request, RequestMessage, Response, ResponseMessage } from 'asn1-extension-protocol';
import { log } from './log';

export type ResponseHandler = (res: ResponseMessage) => void;

export class ExtensionProtocol {
    private seq = 0;
    private pushHandlers: ResponseHandler[] = [];
    private responseHandlers = new Map<number, ResponseHandler>();

    public constructor(private sendImpl: (req: Request) => void) { }

    public send(req: RequestMessage): Promise<ResponseMessage> {
        return new Promise(resolve => {
            const seq = this.seq++;
            this.responseHandlers.set(seq, res => resolve(res));
            this.sendImpl({
                type: 'custom',
                message: {
                    extension: 'asn1',
                    seq,
                    ...req,
                }
            });
        });
    }

    public addPushHandler(handler: ResponseHandler) {
        this.pushHandlers.push(handler);
    }

    public removePushHandler(handler: ResponseHandler) {
        const index = this.pushHandlers.indexOf(handler);
        if (index !== -1) {
            this.pushHandlers.splice(index, 1);
        }
    }

    public handle(res: Response) {
        if (res.type === 'custom' && res.message?.extension === 'asn1') {
            const seq = res.message.seq;
            if (typeof seq === 'undefined') {
                if (this.pushHandlers.length > 0) {
                    for (const pushHandler of this.pushHandlers) {
                        pushHandler(res.message);
                    }
                } else {
                    log.warn(`Received '${res.message.type}' push message, but no push message handlers exist. Discarding push message.`);
                }
                return;
            }
            const handler = this.responseHandlers.get(seq);
            if (!handler) {
                log.warn(`Received '${res.message.type}' response with sequence number ${seq}, but no handler for that sequence number exists. Discarding response.`);
            } else {
                this.responseHandlers.delete(seq);
                handler(res.message);
            }
        }
    }
}
