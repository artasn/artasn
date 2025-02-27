import React, { CSSProperties } from 'react';
import ProjectView from './ProjectView';
import { Box, Tab, Tabs } from '@mui/material';
import PackageRegistryView from './PackageRegistryView';
import { AccountTree, CloudDownload } from '@mui/icons-material';

interface PanelProps {
    index: number;
    value: number;
    style?: CSSProperties;
    children?: React.ReactNode;
}

function TabPanel(props: PanelProps) {
    const { index, value, style, children, ...other } = props;

    return (
        <div
            role="tabpanel"
            hidden={value !== index}
            id={`tabpanel-${index}`}
            aria-labelledby={`tab-${index}`}
            style={{ ...style, height: 'calc(100% - 83px)' }}
            {...other}>
            {value === index && children}
        </div>
    );
}

const TabView = () => {
    const [value, setValue] = React.useState(0);

    const handleChange = (_: React.SyntheticEvent, newValue: number) => {
        setValue(newValue);
    };

    return (
        <Box style={{
            position: 'fixed',
            left: '50vw',
            top: '0px',
            width: '50vw',
            height: '100vh',
            padding: '10px',
        }}>
            <Box sx={{ borderBottom: 1, borderColor: 'divider' }}>
                <Tabs value={value} onChange={handleChange} aria-label="basic tabs example">
                    <Tab label="Project" icon={<AccountTree />} iconPosition="start" />
                    <Tab label="Package Registry" icon={<CloudDownload />} iconPosition="start" />
                </Tabs>
            </Box>
            <TabPanel value={value} index={0}>
                <ProjectView />
            </TabPanel>
            <TabPanel value={value} index={1}>
                <PackageRegistryView />
            </TabPanel>
        </Box>
    );
};

export default TabView;
