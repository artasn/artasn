import React, { useEffect, useState } from 'react';
import VisualStudioCode, { getExtensionHostIFrame } from '../code/VisualStudioCode';
import TabView from './TabView';
import AppLoadingView from './AppLoadingView';

import './App.css';

const App = () => {
    const [loading, setLoading] = useState(true);

    useEffect(() => {
        (async () => {
            // wait for the VSCode window to start, with the extension host loaded
            await getExtensionHostIFrame();
            setLoading(false);
        })();
    }, []);

    return (
        <>
            <VisualStudioCode />
            <TabView />
            <AppLoadingView loading={loading} />
        </>
    );
};

export default App;
