import React, { useEffect, useState } from 'react';
import VisualStudioCode, { getExtensionHostIFrame } from '../code/VisualStudioCode';
import TabView from './TabView';
import AppLoadingView from './AppLoadingView';
import { ToastContainer } from 'react-toastify';

import './App.css';
import { initASN1Extension } from '../code/asn1';

const App = () => {
    const [loading, setLoading] = useState(true);

    useEffect(() => {
        (async () => {
            // wait for the VSCode window to start, with the extension host loaded
            await getExtensionHostIFrame();
            initASN1Extension();
            setLoading(false);
        })();
    }, []);

    return (
        <>
            <VisualStudioCode />
            <TabView />
            <AppLoadingView loading={loading} />
            <ToastContainer />
        </>
    );
};

export default App;
