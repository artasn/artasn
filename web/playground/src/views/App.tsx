import React, { useEffect, useState } from 'react';
import VisualStudioCode, { getExtensionHostIFrame } from '../code/VisualStudioCode';
import TabView from './TabView';
import AppLoadingView from './AppLoadingView';
import SplitDivider, { clampSplitPosition } from '../components/SplitDivider';
import { ToastContainer } from 'react-toastify';

import './App.css';
import { initASN1Extension } from '../code/asn1';

const SPLIT_STORAGE_KEY = 'artasn.splitPosition';
const DEFAULT_SPLIT = 50;
const MIN_SIDE_WIDTH = 300;

function loadSplitPosition(): number {
    try {
        const stored = Number(localStorage.getItem(SPLIT_STORAGE_KEY));
        if (stored > 0 && stored < 100) {
            return clampSplitPosition(stored, MIN_SIDE_WIDTH);
        }
    } catch {
        // localStorage may be unavailable; use the default
    }
    return DEFAULT_SPLIT;
}

const App = () => {
    const [loading, setLoading] = useState(true);
    // the width of the VSCode pane, as a percentage of the window width
    const [split, setSplit] = useState(loadSplitPosition);

    useEffect(() => {
        (async () => {
            // wait for the VSCode window to start, with the extension host loaded
            await getExtensionHostIFrame();
            initASN1Extension();
            setLoading(false);
        })();
    }, []);

    useEffect(() => {
        // VSCode only re-lays out its workbench when the window is resized, not when its container is resized,
        // so notify it that its size changed (at most once per frame while dragging)
        const frame = requestAnimationFrame(() => window.dispatchEvent(new Event('resize')));
        try {
            localStorage.setItem(SPLIT_STORAGE_KEY, String(split));
        } catch {
            // ignore; the position just won't be remembered
        }
        return () => cancelAnimationFrame(frame);
    }, [split]);

    return (
        <>
            <VisualStudioCode width={`${split}vw`} />
            <SplitDivider
                position={split}
                onChange={setSplit}
                onReset={() => setSplit(DEFAULT_SPLIT)}
                minSideWidth={MIN_SIDE_WIDTH} />
            <TabView left={`${split}vw`} width={`${100 - split}vw`} />
            <AppLoadingView loading={loading} />
            <ToastContainer />
        </>
    );
};

export default App;
