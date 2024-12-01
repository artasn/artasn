import React, { useEffect } from 'react';
import EditorConfig from './EditorConfig.json';
import { getWebFS } from '../webfs';

let extensionHostIFrame: HTMLIFrameElement | null = null;
let loading = false;

type LoadCallback = (extensionHostIFrame: HTMLIFrameElement) => void;
const loadCallbacks: LoadCallback[] = [];

declare const window: Window & {
    createWorkbench?: (config: any) => void;
};

export async function getExtensionHostIFrame(): Promise<HTMLIFrameElement> {
    if (extensionHostIFrame !== null) {
        return extensionHostIFrame;
    }
    if (loading) {
        return new Promise(resolve => loadCallbacks.push(resolve));
    }
    loading = true;
    return new Promise(resolve => {
        const handle = setInterval(() => {
            const iframes = document.getElementsByClassName('web-worker-ext-host-iframe');
            if (iframes.length > 0) {
                clearInterval(handle);
                const iframe = iframes[0] as HTMLIFrameElement;
                loading = false;

                resolve(iframe);
                for (const loadCallback of loadCallbacks) {
                    loadCallback(iframe);
                }
            }
        }, 50);
    });
}

const VisualStudioCode = () => {
    useEffect(() => {
        // try to create the workbench every 50ms
        // because the VSCode JS files are loaded asynchronously,
        // window.createWorkbench will not be available until all files are loaded
        const handle = setInterval(async () => {
            if (window.createWorkbench) {
                clearInterval(handle);

                // ensure WebFS is working (i.e. the database is created) before creating the workbench
                await getWebFS();

                window.createWorkbench(EditorConfig);

                // pre-load the extension host iframe
                // this will cache it, so that future calls will be very fast
                await getExtensionHostIFrame();
            }
        }, 50);
    }, []);

    return (
        <>
            <div id="vs-editor" style={{
                zIndex: '999',
                position: 'fixed',
                left: '0px',
                top: '0px',
                width: '50vw',
                height: '100vh',
            }}></div>
        </>
    );
}
export default VisualStudioCode;
