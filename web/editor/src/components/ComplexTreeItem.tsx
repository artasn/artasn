import * as React from 'react';
import clsx from 'clsx';
import { styled, alpha } from '@mui/material/styles';
import Box from '@mui/material/Box';
import { treeItemClasses } from '@mui/x-tree-view/TreeItem';
import { useTreeItem2, UseTreeItem2Parameters } from '@mui/x-tree-view/useTreeItem2';
import {
    TreeItem2Checkbox,
    TreeItem2Content,
    TreeItem2GroupTransition,
    TreeItem2IconContainer,
    TreeItem2Label,
    TreeItem2Root,
} from '@mui/x-tree-view/TreeItem2';
import { TreeItem2Icon } from '@mui/x-tree-view/TreeItem2Icon';
import { TreeItem2Provider } from '@mui/x-tree-view/TreeItem2Provider';
import { TreeItem2DragAndDropOverlay } from '@mui/x-tree-view/TreeItem2DragAndDropOverlay';

export type TreeItemData = {
    label: string;
    id?: string;
    secondaryLabel?: string;
    subtext?: string;
    children?: TreeItemData[];
    icon?: React.ElementType;
    data?: any;
};

declare module 'react' {
    interface CSSProperties {
        '--tree-view-color'?: string;
        '--tree-view-bg-color'?: string;
    }
}

const StyledTreeItemRoot = styled(TreeItem2Root)(({ theme }) => ({
    color: theme.palette.grey[400],
    position: 'relative',
    [`& .${treeItemClasses.groupTransition}`]: {
        marginLeft: theme.spacing(3.5),
    },
    ...theme.applyStyles('light', {
        color: theme.palette.grey[800],
    }),
})) as unknown as typeof TreeItem2Root;

const Content = styled(TreeItem2Content)(({ theme }) => ({
    flexDirection: 'row',
    borderRadius: theme.spacing(0.7),
    marginBottom: theme.spacing(0.5),
    marginTop: theme.spacing(0.5),
    padding: theme.spacing(0.5),
    paddingRight: theme.spacing(1),
    fontWeight: 500,
    '.labelIcon': {
        color: theme.palette.primary.dark,
        ...theme.applyStyles('light', {
            color: theme.palette.primary.main,
        }),
    },
    [`&.Mui-expanded `]: {
        '&::before': {
            content: '""',
            display: 'block',
            position: 'absolute',
            left: '16px',
            top: '44px',
            height: 'calc(100% - 48px)',
            width: '1.5px',
            backgroundColor: theme.palette.grey[700],
            ...theme.applyStyles('light', {
                backgroundColor: theme.palette.grey[300],
            }),
        },
    },
    '&:hover': {
        backgroundColor: alpha(theme.palette.primary.main, 0.1),
        color: 'white',
        ...theme.applyStyles('light', {
            color: theme.palette.primary.main,
        }),
    },
    [`&.Mui-focused, &.Mui-selected, &.Mui-selected.Mui-focused`]: {
        '.labelIcon': {
            color: 'white',
        },
        backgroundColor: theme.palette.primary.dark,
        color: theme.palette.primary.contrastText,
        ...theme.applyStyles('light', {
            backgroundColor: theme.palette.primary.main,
        }),
    },
}));

interface LabelProps {
    children: React.ReactNode;
    secondaryLabel?: string;
    subtext?: string;
    icon?: React.ElementType;
    expandable?: boolean;
}

function Label({
    icon,
    secondaryLabel,
    subtext,
    expandable,
    children,
    ...other
}: LabelProps) {
    return (
        <TreeItem2Label
            {...other}
            sx={{
                display: 'flex',
                flexDirection: 'column',
                alignItems: 'left',
            }}>
            <Box sx={{ display: 'flex', alignItems: 'center', }}>
                {icon && (
                    <Box
                        component={icon}
                        className="labelIcon"
                        color="inherit"
                        sx={{ mr: 1, fontSize: '1.2rem' }} />
                )}
                {children}
                {secondaryLabel && <span style={{
                    marginLeft: '10px',
                    fontSize: '0.8rem',
                    fontFamily: 'Courier New',
                    color: 'grey'
                }}>{secondaryLabel}</span>}
            </Box>
            {subtext && <Box style={{
                marginLeft: '20px',
                fontSize: '0.8rem',
                fontFamily: 'Courier New',
            }}>
                ::= {subtext}
            </Box>}
        </TreeItem2Label>
    );
}

const isExpandable = (reactChildren: React.ReactNode) => {
    if (Array.isArray(reactChildren)) {
        return reactChildren.length > 0 && reactChildren.some(isExpandable);
    }
    return Boolean(reactChildren);
};

interface ComplexTreeItemProps
    extends Omit<UseTreeItem2Parameters, 'rootRef'>,
    Omit<React.HTMLAttributes<HTMLLIElement>, 'onFocus'> { }

const ComplexTreeItem = React.forwardRef(function CustomTreeItem(
    props: ComplexTreeItemProps,
    ref: React.Ref<HTMLLIElement>,
) {
    const { id, itemId, label, disabled, children, ...other } = props;

    const {
        getRootProps,
        getContentProps,
        getIconContainerProps,
        getCheckboxProps,
        getLabelProps,
        getGroupTransitionProps,
        getDragAndDropOverlayProps,
        status,
        publicAPI,
    } = useTreeItem2({ id, itemId, children, label, disabled, rootRef: ref });

    const item = publicAPI.getItem(itemId);
    const expandable = isExpandable(children);
    let { icon, secondaryLabel, subtext } = item as TreeItemData;

    return (
        <TreeItem2Provider itemId={itemId}>
            <StyledTreeItemRoot {...getRootProps(other)}>
                <Content
                    {...getContentProps({
                        className: clsx('content', {
                            'Mui-expanded': status.expanded,
                            'Mui-selected': status.selected,
                            'Mui-focused': status.focused,
                            'Mui-disabled': status.disabled,
                        }),
                    })}>
                    <TreeItem2IconContainer {...getIconContainerProps()}>
                        <TreeItem2Icon status={status} />
                    </TreeItem2IconContainer>
                    <TreeItem2Checkbox {...getCheckboxProps()} />
                    <Label
                        {...getLabelProps({ icon, secondaryLabel, subtext, expandable: expandable && status.expanded })} />
                    <TreeItem2DragAndDropOverlay {...getDragAndDropOverlayProps()} />
                </Content>
                {children && <TreeItem2GroupTransition {...getGroupTransitionProps()} />}
            </StyledTreeItemRoot>
        </TreeItem2Provider>
    );
});
export default ComplexTreeItem;
