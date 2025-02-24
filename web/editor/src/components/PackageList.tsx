import React, { useState } from 'react';
import { Box, Button, Checkbox, FormControlLabel, IconButton, ListItem, ListItemButton, ListItemIcon, ListItemText, SvgIcon, TextField, Typography } from '@mui/material';
import { Package, packageRegistry, Registry, Source } from '../registry';
import { FixedSizeList, ListChildComponentProps } from 'react-window';
import { ArrowBack, FileDownloadDone } from '@mui/icons-material';
import PackageInfo from './PackageInfo';

export interface PackageListProps {
    source?: Source;
    registry?: Registry;
    onClose: () => void;
}

const PackageList = ({ source, registry, onClose, ref, }: PackageListProps & React.RefAttributes<null>) => {
    if (!source || !registry) {
        return <div ref={ref} style={{ display: 'none', }}></div>;
    }

    const [packageFilter, setPackageFilter] = useState('');
    const [selectedPackage, setSelectedPackage] = useState<Package | null>(null);
    const [downloadedOnly, setDownloadedOnly] = useState(false);
    const [downloadedLookup, setDownloadedLookup] = useState((() => {
        const downloaded = packageRegistry.getDownloadedPackages();
        const downloadedLookup = new Map<string, null>();
        for (const info of downloaded) {
            if (info.source === source.name) {
                downloadedLookup.set(info.name, null);
            }
        }
        return downloadedLookup;
    })());

    let filteredPackages = registry;
    if (downloadedOnly) {
        filteredPackages = filteredPackages.filter(pkg => downloadedLookup.has(pkg.name));
    }
    if (packageFilter !== '') {
        filteredPackages = filteredPackages.filter(pkg => pkg.name.includes(packageFilter) || pkg.title?.includes(packageFilter));
    }
    filteredPackages.sort((a, b) => {
        const downloadedA = downloadedLookup.has(a.name);
        const downloadedB = downloadedLookup.has(b.name);
        if (downloadedA === downloadedB) { // if they are both downloaded or both not downloaded
            return a.name.localeCompare(b.name);
        }
        if (downloadedA) {
            return -1;
        }
        if (downloadedB) {
            return 1;
        }

        throw new Error('unreachable');
    });

    const onDownloadStateChanged = (pkg: string, state: boolean) => {
        if (state) {
            setDownloadedLookup(new Map(downloadedLookup).set(pkg, null));
        } else {
            const copy = new Map(downloadedLookup);
            copy.delete(pkg);
            setDownloadedLookup(copy);
        }
    };

    const PackageRow = (props: ListChildComponentProps) => {
        const { index, style } = props;
        const pkg = filteredPackages![index];

        const isDownloaded = downloadedLookup.has(pkg.name);

        const onSelected = () => {
            if (selectedPackage?.name === pkg.name) {
                setSelectedPackage(null);
            } else {
                setSelectedPackage(pkg);
            }
        };

        return (
            <ListItem style={style} key={index} component="div" disablePadding>
                <ListItemButton style={{ height: '100%', }} onClick={onSelected} selected={selectedPackage?.name === pkg.name}>
                    {isDownloaded && <ListItemIcon><SvgIcon component={FileDownloadDone} /></ListItemIcon>}
                    <ListItemText primary={pkg.name} secondary={pkg.title} />
                </ListItemButton>
            </ListItem>
        );
    };

    return (
        <Box ref={ref} sx={{ display: 'flex', flexDirection: 'row', width: '100%', height: '100%', }}>
            <Box sx={{
                flexGrow: 1,
                flexBasis: 0,
                borderRight: '1px solid lightgrey',
                padding: '10px',
            }}>
                <Button variant="outlined" startIcon={<ArrowBack />} onClick={onClose}>Return to Sources</Button>
                <Box sx={{ marginTop: '10px', }}>
                    <Typography variant="h6">{source.name} Packages</Typography>
                    <FormControlLabel control={
                        <Checkbox value={downloadedOnly} onChange={event => setDownloadedOnly(event.currentTarget.checked)} />}
                        label="Include Downloaded Only" />
                    <TextField
                        label={`search ${registry.length} packages from ${source.name}`}
                        variant="outlined"
                        fullWidth
                        value={packageFilter}
                        onChange={event => setPackageFilter(event.currentTarget.value)} />
                </Box>
                <Box sx={{ marginTop: '10px', }}>
                    {filteredPackages!.length === 0 ? (
                        <Typography style={{ height: 600, textAlign: 'center', }}>No packages match the given filters</Typography>
                    ) : (
                        <FixedSizeList
                            width="100%"
                            height={600}
                            itemSize={100}
                            itemCount={filteredPackages!.length}
                            overscanCount={5}>
                            {PackageRow}
                        </FixedSizeList>
                    )}
                </Box>
            </Box>
            <Box sx={{
                flexGrow: 1,
                flexBasis: 0,
                padding: '10px',
            }}>
                {selectedPackage === null ? (
                    <Typography variant="body1">Select a package for details and downloading</Typography>
                ) : (
                    <PackageInfo
                        source={source}
                        pkg={selectedPackage}
                        onDownloadStateChanged={state => onDownloadStateChanged(selectedPackage.name, state)} />
                )}
            </Box>
        </Box>
    );
};

export default PackageList;
