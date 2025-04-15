import React, { useEffect, useState } from 'react';
import { RichTreeView } from '@mui/x-tree-view/RichTreeView';
import { getWebFS } from '../webfs';
import * as compiler from '../compiler';
import { Grid2 as Grid, Box, Card, Checkbox, FormControlLabel, Switch } from '@mui/material';
import { ModuleIdentifier, QualifiedIdentifier, TagClass, BuiltinType, TypeDefinition, TaggedType, ValueReference, TagSource, isCharacterStringType, TransferSyntax } from '../wasm-definitions';
import ComplexTreeItem, { TreeItemData } from '../components/ComplexTreeItem';
import IconModule from '../icons/IconModule';
import IconType from '../icons/IconType';
import IconValue from '../icons/IconValue';
import DecodedValueInfo, { DecodedValueViewMode } from '../components/DecodedValueInfo';
import { stringifyJSON } from '../util';

function getModuleString(module: ModuleIdentifier): string {
    if (module.oid) {
        return `${module.name} (${module.oid})`;
    } else {
        return module.name;
    }
}

function getTypeString(ty: TaggedType): string {
    if (ty.mode === 'reference') {
        return ty.name + ' FROM ' + getModuleString(ty.module);
    } else {
        let str;
        if (ty.tag && ty.tag.source !== TagSource.TagImplied) {
            if (ty.tag.class === TagClass.ContextSpecific) {
                str = `[${ty.tag.num}] `;
            } else {
                str = `[${ty.tag.class} ${ty.tag.num}] `;
            }
            if (ty.tag.source !== TagSource.KindImplied) {
                str += `${ty.tag.kind} `;
            }
            str += ty.type;
        } else {
            str = ty.type;
        }
        if (ty.type === 'SEQUENCE OF') {
            str += ` ${getTypeString(ty.componentType)}`;
        }
        return str;
    }
}

function getValueString(ref: ValueReference): string {
    if (ref.mode === 'reference') {
        return ref.name + ' FROM ' + getModuleString(ref.module);
    } else if (ref.mode === 'value') {
        switch (ref.type) {
            case 'BOOLEAN':
                return ref.value.toString().toUpperCase();
            case 'INTEGER':
            case 'REAL':
                return ref.value.toString();
            case 'BIT STRING':
                return ref.value.data.slice(0, ref.value.data.length - ref.value.unusedBits);
            case 'OBJECT IDENTIFIER':
                return ref.value;
            case 'OCTET STRING':
                return ref.value.toUpperCase();
            case 'NULL':
                return 'NULL';
            default:
                if (isCharacterStringType(ref.type)) {
                    return `"${(ref as any).value}"`;
                }
                return '';
        }
    } else if (ref.mode === 'objectClassField') {
        return `${ref.class}.&${ref.field}`;
    } else {
        throw new Error('unreachable');
    }
}

function qualifiedIdentifierEquals(a: QualifiedIdentifier, b: QualifiedIdentifier): boolean {
    if (a.name === b.name) {
        return a.module.name === b.module.name && a.module.oid === b.module.oid;
    }
    return false;
}

function getTypeItem(id: string, ty: TaggedType, name: string): TreeItemData {
    let label = name;
    let secondaryLabel = getTypeString(ty);

    let children: TreeItemData[] | undefined;
    if (ty.mode === 'type') {
        if (ty.type === 'SEQUENCE') {
            children = [];
            for (const component of ty.components) {
                children.push(getTypeItem(`${id}/${component.name}`, component.componentType, component.name));
            }
        } else if (ty.type === 'CHOICE') {
            children = [];
            for (const alternative of ty.alternatives) {
                children.push(getTypeItem(`${id}/${alternative.name}`, alternative.alternativeType, alternative.name));
            }
        }
    }

    return {
        id,
        label,
        secondaryLabel,
        children,
    };
}

function getHierachicalTree(declarations: compiler.Declarations): TreeItemData[] {
    const typeItems: TreeItemData[] = [];

    const types = new Set(declarations.values.map(value => JSON.stringify(value.ty)));
    for (const type of types) {
        let children: TreeItemData[] = [];
        const values = declarations.values.filter(value => JSON.stringify(value.ty) === type);
        for (const value of values) {
            let label = value.ident.name;
            children.push({
                label,
            });
        }

        let label = getTypeString(JSON.parse(type));
        typeItems.push({
            label,
            children,
        });
    }

    return typeItems;
}

function getFlatTree(declarations: compiler.Declarations): TreeItemData[] {
    const items: TreeItemData[] = [];
    {
        const modulesItem: TreeItemData = {
            label: 'Modules',
            children: [],
            icon: IconModule,
        };
        for (const module of declarations.modules) {
            modulesItem.children!.push({
                label: module.name,
                secondaryLabel: module.oid,
                data: { kind: 'module', module },
            });
        }
        items.push(modulesItem);
    }
    {
        const typesItem: TreeItemData = {
            label: 'Types',
            children: [],
            icon: IconType,
        };
        for (const typeDef of declarations.types) {
            typesItem.children!.push({
                ...getTypeItem(`types/${JSON.stringify(typeDef.ident)}`, typeDef.ty, typeDef.ident.name),
                data: { kind: 'type', ident: typeDef.ident }
            });
        }
        items.push(typesItem);
    }
    {
        const valuesItem: TreeItemData = {
            label: 'Values',
            children: [],
            icon: IconValue,
        };
        for (const valueDef of declarations.values) {
            let type = valueDef.ty.mode === 'reference' && {
                module: valueDef.ty.module,
                name: valueDef.ty.name,
            };

            valuesItem.children!.push({
                id: `values/${JSON.stringify(valueDef.ident)}`,
                label: valueDef.ident.name,
                secondaryLabel: getTypeString(valueDef.ty),
                data: { kind: 'value', ident: valueDef.ident, type },
            });
        }
        items.push(valuesItem);
    }
    return items;
}

function sortTree(items: TreeItemData[]) {
    items.sort((a, b) => a.label.localeCompare(b.label));
}

const ProjectView = () => {
    const [compiling, setCompiling] = useState(false);
    const [tree, setTree] = useState<{
        hierarchical: boolean;
        declarations: compiler.Declarations | null;
        items: TreeItemData[];
    }>({
        hierarchical: false,
        declarations: null,
        items: [],
    });
    const [selectedItemType, setSelectedItemType] = useState<QualifiedIdentifier | null>(null);
    const [viewMode, setViewMode] = useState<DecodedValueViewMode>('components');
    const [derValue, setDerValue] = useState('');

    const refreshTree = (res: compiler.CompileResult) => {
        if (res.kind === 'error') {
            setTree({
                hierarchical: tree.hierarchical,
                declarations: null,
                items: [],
            });
            setDerValue('');
            setSelectedItemType(null);
        } else {
            setTreeData(tree.hierarchical, res.declarations);
        }
        setCompiling(false);
    };

    const setTreeData = (hierarchical: boolean, newDeclarations?: compiler.Declarations) => {
        const declarations = newDeclarations ?? tree.declarations;

        let items: TreeItemData[];
        if (declarations !== null) {
            if (hierarchical) {
                items = getHierachicalTree(declarations);
            } else {
                items = getFlatTree(declarations);
            }
        } else {
            items = [];
        }

        for (const rootItem of items) {
            sortTree(rootItem.children!);
        }

        setTree({
            hierarchical,
            declarations,
            items,
        });
    };

    const onTreeItemSelected = async (_: React.SyntheticEvent, itemId: string | null) => {
        if (itemId !== null) {
            const parsed = JSON.parse(itemId);
            if (parsed.data) {
                if (parsed.data.kind === 'value') {
                    const ident: QualifiedIdentifier = parsed.data.ident;
                    setSelectedItemType(parsed.data.type);
                    setDerValue(await compiler.encodeValue(TransferSyntax.DER, ident));
                }
            } else {
                setDerValue('');
                setSelectedItemType(null);
            }
        }
    };

    useEffect(() => {
        // compile files on load
        (async () => {
            setCompiling(true);
            const res = await compiler.getDeclarations();
            setCompiling(false);
            refreshTree(res);
        })();

        // when compilation events happen, handle them
        // these are triggered automatically by changes to files
        // see ./compiler/compiler.ts for the event listener that does this
        compiler.addEventListener(event => {
            if (event.kind === 'compiling') {
                setCompiling(true);
            } else if (event.kind === 'result') {
                setCompiling(false);
                refreshTree(event.result);
            }
        });
    }, []);

    return (
        <Box sx={{
            display: 'flex',
            flexDirection: 'row',
            height: '100%',
            gap: '10px',
            padding: '10px',
        }}>
            <Box sx={{
                display: 'flex',
                flexDirection: 'column',
                flexGrow: 1,
                flexBasis: 0,
                overflowY: 'scroll',
                marginBottom: '50px',
            }}>
                <Box sx={{ flexGrow: 1 }}>
                    <FormControlLabel
                        control={<Checkbox
                            checked={tree.hierarchical}
                            onChange={event => setTreeData(event.currentTarget.checked)} />}
                        label="Hierarchical View" />
                    <RichTreeView
                        items={tree.items}
                        slots={{ item: ComplexTreeItem }}
                        getItemId={item => stringifyJSON(item)}
                        onSelectedItemsChange={onTreeItemSelected} />
                </Box>
                {compiling ? (
                    <Box style={{
                        margin: '0px 0px 20px 20px',
                        fontSize: '1rem',
                    }}>Compiling...</Box>
                ) : null}
            </Box>
            <Box sx={{
                marginBottom: '50px',
                flexGrow: 1,
                flexBasis: 0,
                maxWidth: '50%',
            }}>
                <Card style={{
                    overflow: 'scroll',
                    padding: '10px',
                    height: '100%',
                    maxWidth: '100%',
                    fontSize: '1rem',
                    userSelect: 'text',
                }}>
                    <Grid component="label" container alignItems="center" justifyContent="center" spacing={1}>
                        <Grid>TLV</Grid>
                        <Grid>
                            <Switch
                                checked={viewMode === 'components'}
                                onChange={() => setViewMode(viewMode === 'components' ? 'tlv' : 'components')}
                                disabled={derValue === ''} />
                        </Grid>
                        <Grid>Components</Grid>
                    </Grid>
                    <DecodedValueInfo typeIdent={selectedItemType ?? undefined} viewMode={viewMode} encodedValue={derValue} />
                </Card>
            </Box>
        </Box>
    );
}

export default ProjectView;
