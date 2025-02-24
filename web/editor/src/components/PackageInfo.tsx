import React, { useState } from 'react';
import { Box, Button, Dialog, DialogActions, DialogContent, DialogContentText, DialogTitle, Link, Typography } from '@mui/material';
import { Package, packageRegistry, Source } from '../registry';
import { Delete, Download } from '@mui/icons-material';
import { getWebFS } from '../webfs';
import LoadingSpinner from './LoadingSpinner';

export interface PackageInfoProps {
    source: Source;
    pkg: Package;
    onDownloadStateChanged: (downloaded: boolean) => void;
}

function basename(path: string): string {
    if (path.includes('/')) {
        return path.substring(path.lastIndexOf('/') + 1);
    } else {
        return path;
    }
}

const PackageInfo = ({ source, pkg, onDownloadStateChanged }: PackageInfoProps) => {
    const [downloadModalOpen, setDownloadModalOpen] = useState(false);
    const [currentDownload, setCurrentDownload] = useState<string | null>(null);
    const [finished, setFinished] = useState(false);
    const [isDownloaded, setIsDownloaded] = useState((() => {
        const downloaded = packageRegistry.getDownloadedPackages();
        return downloaded.find(dpkg => dpkg.source === source.name && dpkg.name === pkg.name) ?? null !== null;
    })());

    let name = pkg.name;
    if (pkg.approval) {
        name = `${name} (${pkg.approval})`;
    }
    let modules = pkg.modules;
    const updatedBy = [];
    if (pkg.updates) {
        for (const update of pkg.updates) {
            updatedBy.push(update.name);
            modules.push(...update.modules);
        }
    }

    const downloadModules = () => {
        (async () => {
            const fs = await getWebFS();
            if (!fs.exists('/packages')) {
                await fs.mkdir('/packages');
            }

            const pkgDir = `/packages/${pkg.name}`;
            if (!fs.exists(pkgDir)) {
                await fs.mkdir(pkgDir);
            }

            for (const module of modules) {
                setCurrentDownload(module);
                const moduleBinary = await packageRegistry.downloadModule(source.name, module);

                await fs.writeFile(`${pkgDir}/${basename(module)}`, new Uint8Array(moduleBinary), { create: true, overwrite: true, });
            }

            packageRegistry.savePackage(source.name, pkg.name);

            setCurrentDownload(null);
            setFinished(true);
            setIsDownloaded(true);
            onDownloadStateChanged(true);
        })();
    };

    const deletePackage = () => {
        (async () => {
            const fs = await getWebFS();
            const pkgDir = `/packages/${pkg.name}`;

            if (fs.exists(pkgDir)) {
                for (const module of modules) {
                    const path = `${pkgDir}/${basename(module)}`;
                    if (fs.exists(path)) {
                        await fs.delete(path);
                    }
                }
                await fs.delete(pkgDir);
            }

            packageRegistry.deletePackage(source.name, pkg.name);

            setIsDownloaded(false);
            onDownloadStateChanged(false);
        })();
    };

    const closeModal = () => {
        if (finished) {
            setFinished(false);
        }
        setDownloadModalOpen(false);
    };

    const getDialogContent = () => {
        if (finished) {
            return (
                <>
                    <DialogTitle>Downloaded {pkg.name}!</DialogTitle>
                    <DialogContent>
                        <DialogContentText>All package modules have been successfully downloaded.</DialogContentText>
                    </DialogContent>
                    <DialogActions>
                        <Button onClick={closeModal}>OK</Button>
                    </DialogActions>
                </>
            );
        }
        if (currentDownload === null) {
            return (
                <>
                    <DialogTitle>Download {pkg.name} ASN.1 Modules</DialogTitle>
                    <DialogContent>
                        <Box>
                            <Typography>The {pkg.name} package contains {modules.length} module(s):</Typography>
                            <Box sx={{ marginLeft: '20px' }}>
                                {modules.map(module => <Typography key={module}>{basename(module)}</Typography>)}
                            </Box>
                            <Typography>Do you want to download this package?</Typography>
                        </Box>
                    </DialogContent>
                    <DialogActions>
                        <Button onClick={closeModal}>Cancel</Button>
                        <Button onClick={downloadModules}>Confirm Download</Button>
                    </DialogActions>
                </>
            );
        } else {
            return (
                <>
                    <DialogTitle>Downloading {pkg.name} ASN.1 Modules</DialogTitle>
                    <DialogContent>
                        <Box sx={{ display: 'flex', alignItems: 'center', gap: '10px', }}>
                            <LoadingSpinner />
                            <Typography>
                                Downloading {currentDownload}...
                            </Typography>
                        </Box>
                    </DialogContent>
                </>
            );
        }
    };

    return (
        <>
            <Box sx={{ marginBottom: '10px', }}>
                <Typography variant="h6">
                    {pkg.url ? <Link href={pkg.url} target="_blank">{name}</Link> : name}
                    {pkg.title && <span>: {pkg.title}</span>}
                </Typography>
                <Typography variant="body2">Contains {modules.length} ASN.1 module definition(s)</Typography>
                {updatedBy.length > 0 && <Typography variant="caption">Updated by {updatedBy.join(', ')}</Typography>}
            </Box>
            {isDownloaded ? (
                <Button variant="outlined" startIcon={<Delete />} onClick={deletePackage}>
                    Delete Package
                </Button>
            ) : (
                <Button variant="outlined" startIcon={<Download />} onClick={() => setDownloadModalOpen(true)}>
                    Download Package
                </Button>
            )}
            <Dialog onClose={closeModal} open={downloadModalOpen}>
                {getDialogContent()}
            </Dialog>
        </>
    );
};


export default PackageInfo;
