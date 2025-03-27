import React, { useState } from 'react';
import { Box, Button, Dialog, DialogActions, DialogContent, DialogContentText, DialogTitle, Link, Typography } from '@mui/material';
import { Module, Package, PackageDependency, packageRegistry, Registry, Source } from '../registry';
import { Delete, Download } from '@mui/icons-material';
import { getWebFS } from '../webfs';
import LoadingSpinner from './LoadingSpinner';
import ObjectSet from '../util/ObjectSet';

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

function getPackageModules(registries: { source: Source, registry: Registry }[], pkg: PackageDependency): {
    modules: Module[];
    updatedBy: string[];
} {
    const { registry } = registries.find(({ source }) => source.name === pkg.source)!;
    const registryPkg = registry.find(registryPkg => registryPkg.name === pkg.name)!;

    const modules = new ObjectSet<Module>((module) => `${module.name}.${module.oid}`, registryPkg.modules);
    const updatedBy = [];
    if (registryPkg.updates) {
        for (const update of registryPkg.updates) {
            updatedBy.push(update.name);
            modules.add(...update.modules);
        }
    }

    return {
        modules: modules.sortedArray(),
        updatedBy,
    };
};

const PackageInfo = ({ source, pkg, onDownloadStateChanged }: PackageInfoProps) => {
    const [downloadModalOpen, setDownloadModalOpen] = useState(false);
    const [currentDownload, setCurrentDownload] = useState<string | null>(null);
    const [finished, setFinished] = useState(false);
    const [isDownloaded, setIsDownloaded] = useState((() => {
        const downloaded = packageRegistry.getDownloadedPackages();
        return downloaded.find(dpkg => dpkg.source === source.name && dpkg.name === pkg.name) ?? null !== null;
    })());

    const registries = packageRegistry.getSources().map(source => ({
        source,
        registry: packageRegistry.getRegistry(source.name),
    }));

    let name = pkg.name;
    if (pkg.approval) {
        name = `${name} (${pkg.approval})`;
    }

    const { modules, updatedBy } = getPackageModules(registries, {
        source: source.name,
        name: pkg.name,
    });

    const deepDependencies = new ObjectSet<PackageDependency>((object) => `${object.source}.${object.name}`, pkg.dependencies);
    if (deepDependencies.size > 0) {

        // continue finding dependencies and adding them to the set
        // once no more dependencies are found, break from the loop
        // this is not very optimized, but it works fine for now
        while (true) {
            const ddSize = deepDependencies.size;
            for (const { source, registry } of registries) {
                for (const otherPkg of registry) {
                    if (deepDependencies.has({
                        source: source.name,
                        name: otherPkg.name,
                    })) {
                        if (otherPkg.dependencies) {
                            for (const deepDependency of otherPkg.dependencies) {
                                deepDependencies.add(deepDependency);
                            }
                        }
                    }
                }
            }
            if (deepDependencies.size === ddSize) {
                break;
            }
        }
        // if there is a backreference to this package, delete it from the set
        deepDependencies.delete({
            source: source.name,
            name: pkg.name,
        });
    }

    const openDownloadModal = () => {
        downloadModules();
        setDownloadModalOpen(true);
    };

    const downloadModules = () => {
        (async () => {
            const fs = await getWebFS();
            if (!fs.exists('/packages')) {
                await fs.mkdir('/packages');
            }

            const packages: PackageDependency[] = [{
                source: source.name,
                name: pkg.name,
            }, ...deepDependencies.array()];

            for (const pkg of packages) {
                const pkgDir = `/packages/${pkg.name}`;
                if (!fs.exists(pkgDir)) {
                    await fs.mkdir(pkgDir);
                }

                for (const module of getPackageModules(registries, pkg).modules) {
                    setCurrentDownload(module.path);
                    const moduleBinary = await packageRegistry.downloadModule(pkg.source, module.path);

                    await fs.writeFile(`${pkgDir}/${basename(module.path)}`, new Uint8Array(moduleBinary), { create: true, overwrite: true, });
                }

                packageRegistry.savePackage(pkg.source, pkg.name);
            }

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
                    const path = `${pkgDir}/${basename(module.path)}`;
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
                    <DialogTitle>Downloaded Modules</DialogTitle>
                    <DialogContent>
                        <DialogContentText>All modules have been successfully downloaded.</DialogContentText>
                    </DialogContent>
                    <DialogActions>
                        <Button onClick={closeModal}>OK</Button>
                    </DialogActions>
                </>
            );
        }
        return (
            <>
                <DialogTitle>Downloading ASN.1 Modules</DialogTitle>
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
    };

    return (
        <>
            <Box sx={{ marginBottom: '10px', }}>
                <Typography variant="h6">
                    {pkg.url ? <Link href={pkg.url} target="_blank">{name}</Link> : name}
                    {pkg.title && <span>: {pkg.title}</span>}
                </Typography>
                {updatedBy.length > 0 && <Typography variant="caption">Updated by {updatedBy.join(', ')}</Typography>}

                <Typography>The {pkg.name} package contains {modules.length} module(s):</Typography>
                <Box sx={{ marginLeft: '20px' }}>
                    {modules.map(module => <Typography key={module.path}>{basename(module.path)}</Typography>)}
                </Box>

                {pkg.dependencies && (
                    <>
                        <Typography>{pkg.name} has {deepDependencies.size} dependencies:</Typography>
                        <Box sx={{ marginLeft: '20px' }}>
                            {deepDependencies.sortedArray().map(dep =>
                                <Typography key={`${dep.source}.${dep.name}`}>{dep.source}/{dep.name}</Typography>
                            )}
                        </Box>
                    </>
                )}
            </Box>
            {isDownloaded ? (
                <Button variant="outlined" startIcon={<Delete />} onClick={deletePackage}>
                    Delete Package
                </Button>
            ) : (
                <Button variant="outlined" startIcon={<Download />} onClick={openDownloadModal}>
                    Download Package
                </Button>
            )}
            <Dialog onClose={closeModal} open={downloadModalOpen} maxWidth="sm" fullWidth>
                {getDialogContent()}
            </Dialog>
        </>
    );
};


export default PackageInfo;
