import React, { useEffect, useState } from 'react';
import { Box, Slide, Typography } from '@mui/material';
import { packageRegistry, Registry, Source, UpdateState } from '../registry';
import PackageSources from '../components/PackageSourceList';
import PackageList from '../components/PackageList';
import LoadingSpinner from '../components/LoadingSpinner';

const PackageRegistryView = () => {
    const [updateState, setUpdateState] = useState<UpdateState>({ kind: 'checking' });
    const [sources, setSources] = useState<Source[]>([]);
    const [selectedSource, setSelectedSource] = useState<{
        source: Source;
        registry: Registry;
    } | null>(null);

    const onSourceSelected = (source: Source | null) => {
        if (source === null) {
            setSelectedSource(null);
        } else {
            const registry = packageRegistry.getRegistry(source.name);
            setSelectedSource({ source, registry, });
        }
    };

    useEffect(() => {
        packageRegistry.pullUpdates(setUpdateState).then(() => {
            setSources(packageRegistry.getSources());
        });
    }, []);

    return (
        <Box sx={{ height: '100%', }}>
            {updateState.kind !== 'inactive' ? (
                <Box sx={{ display: 'flex', alignItems: 'center', gap: '10px', padding: '10px', }}>
                    <LoadingSpinner />
                    <Typography variant="h6">
                        {updateState.kind === 'checking' && 'Checking for updates...'}
                        {updateState.kind === 'fetching-sources' && 'Fetching package sources...'}
                        {updateState.kind === 'fetching-registry' && `Fetching ${updateState.registry} package list...`}
                    </Typography>
                </Box>
            ) : (
                <>
                    <Slide direction="left" in={selectedSource !== null} mountOnEnter unmountOnExit>
                        <PackageList
                            source={selectedSource?.source}
                            registry={selectedSource?.registry}
                            onClose={() => setSelectedSource(null)} />
                    </Slide>
                    <Slide direction="left" in={selectedSource === null} appear mountOnEnter unmountOnExit>
                        <PackageSources
                            sources={sources}
                            onSourceSelected={onSourceSelected} />
                    </Slide>
                </>
            )}
        </Box>
    );
};

export default PackageRegistryView;
