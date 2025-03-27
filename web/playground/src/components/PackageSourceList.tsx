import React, { useState } from 'react';
import { Box, List, ListItem, ListItemButton, ListItemIcon, ListItemText, SvgIcon, Typography } from '@mui/material';
import { Source } from '../registry';
import ChevronRightIcon from '@mui/icons-material/ChevronRight';
import IETFIcon from '../icons/IETF.png';
import ITUIcon from '../icons/ITU.png';

export interface PackageSourcesProps {
    sources: Source[];
    onSourceSelected: (source: Source | null) => void;
}

function getListItemIcon(source: string): string {
    if (source === 'IETF') {
        return IETFIcon;
    } else if (source === 'ITU-T') {
        return ITUIcon;
    } else {
        throw new Error(`invalid source: ${source}`);
    }
}

const PackageSources = ({ sources, onSourceSelected, ref }: PackageSourcesProps & React.RefAttributes<null>) => {
    const setPackageSource = (sourceName: string) => {
        onSourceSelected(sources.find(source => source.name === sourceName)!);
    };

    return (
        <Box ref={ref} sx={{ padding: '10px' }}>
            <Typography variant="h5">Package Sources</Typography>
            <Typography variant="caption">Standards organizations whose recommendations contain ASN.1 modules definitions in common use today.</Typography>
            <List disablePadding>
                {sources.map(source => (
                    <ListItem key={source.name} disablePadding onClick={() => setPackageSource(source.name)}>
                        <ListItemButton>
                            <Box sx={{ display: 'flex', flexDirection: 'row', alignItems: 'center', width: '100%', }}>
                                <Box sx={{ display: 'flex', flexDirection: 'row', alignItems: 'start', gap: '10px', }}>
                                    <ListItemIcon>
                                        <img width="64px" src={getListItemIcon(source.name)} />
                                    </ListItemIcon>
                                    <ListItemText primary={
                                        <>
                                            <Typography variant="h6">{source.name}</Typography>
                                            <Typography variant="caption">{source.fullName}</Typography>
                                        </>
                                    } secondary={source.desc} />
                                </Box>
                                <Box sx={{ display: 'flex', flexGrow: 1, flexDirection: 'row', alignContent: 'end', }}>
                                    <SvgIcon style={{ marginLeft: 'auto', }} component={ChevronRightIcon} inheritViewBox />
                                </Box>
                            </Box>
                        </ListItemButton>
                    </ListItem>
                ))}
            </List>
        </Box>
    );
};

export default PackageSources;
