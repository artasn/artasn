import React, { useEffect, useState } from 'react';
import { RichTreeView } from '@mui/x-tree-view/RichTreeView';
import { getWebFS } from '../webfs';
import * as compiler from '../compiler';
import { Box, Card, Checkbox, FormControlLabel } from '@mui/material';
import { CompileError, ModuleIdentifier, QualifiedIdentifier, TagClass, Type, TypeDefinition, TypeReference, ValueDefinition, ValueReference } from '../wasm-definitions';
import ComplexTreeItem, { TreeItemData } from '../components/ComplexTreeItem';
import IconModule from '../icons/IconModule';
import IconType from '../icons/IconType';
import IconValue from '../icons/IconValue';

function getModuleString(module: ModuleIdentifier): string {
    if (module.oid) {
        return `${module.name} (${module.oid})`;
    } else {
        return module.name;
    }
}

function getTypeString(ref: TypeReference): string {
    if (ref.mode === 'reference') {
        return ref.name + ' FROM ' + getModuleString(ref.module);
    } else {
        if (ref.tag.class !== TagClass.ContextSpecific || ref.tag.num) {
            if (ref.tag.class === TagClass.ContextSpecific) {
                return `[${ref.tag.num}] ${ref.kind} ${ref.ty.kind}`;
            } else {
                return `[${ref.tag.class} ${ref.tag.num}] ${ref.kind} ${ref.ty.kind}`;
            }
        }
        return ref.ty.kind;
    }
}

function getValueString(ref: ValueReference): string | undefined {
    if (ref.mode === 'reference') {
        return ref.name + ' FROM ' + getModuleString(ref.module);
    } else {
        switch (ref.kind) {
            case 'BOOLEAN':
                return ref.value.toString().toUpperCase();
            case 'INTEGER':
            case 'REAL':
                return ref.value.toString();
            case 'BIT STRING':
            case 'OBJECT IDENTIFIER':
                return ref.value;
            case 'OCTET STRING':
                return ref.value.toUpperCase();
            case 'NULL':
                return 'NULL';
        }
    }
}

function qualifiedIdentifierEquals(a: QualifiedIdentifier, b: QualifiedIdentifier): boolean {
    if (a.name === b.name) {
        return a.module.name === b.module.name && a.module.oid === b.module.oid;
    }
    return false;
}

function resolveType(typeDefs: TypeDefinition[], type: TypeReference): Type | null {
    if (type.mode === 'type') {
        return type.ty;
    } else {
        const resolved = typeDefs.find(typeDef => qualifiedIdentifierEquals(typeDef.ident, type)) ?? null;
        if (resolved === null) {
            return null;
        }
        return resolveType(typeDefs, resolved.ty);
    }
}

function getValueItem(name: string, ref: ValueReference, type?: TypeReference): TreeItemData {
    let children: TreeItemData[] | undefined;
    if (ref.mode === 'value') {
        if (ref.kind === 'SEQUENCE') {
            children = [];
            for (const component of ref.components) {
                children.push(getValueItem(component.name, component.value));
            }
        }
    }

    return {
        label: name,
        secondaryLabel: type && getTypeString(type),
        subtext: getValueString(ref),
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
            let children: TreeItemData[] | undefined;
            if (typeDef.ty.mode === 'type') {
                const { ty } = typeDef.ty;
                if (ty.kind === 'SEQUENCE') {
                    children = [];
                    for (const component of ty.components) {
                        children.push({
                            label: component.name,
                            secondaryLabel: getTypeString(component.componentType),
                        });
                    }
                }
            }

            let label = typeDef.ident.name;
            typesItem.children!.push({
                label,
                secondaryLabel: getTypeString(typeDef.ty),
                children,
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
            valuesItem.children!.push({
                ...getValueItem(valueDef.ident.name, valueDef.value, valueDef.ty),
                data: { kind: 'value', ident: valueDef.ident },
            });
        }
        items.push(valuesItem);
    }
    return items;
}

function sortTree(items: TreeItemData[]) {
    for (const item of items) {
        if (item.children) {
            sortTree(item.children);
        }
    }
    items.sort((a, b) => a.label.localeCompare(b.label));
}

const ProjectView = () => {
    const derHelpText = 'Select a value to inspect its DER encoding';

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
    const [derValue, setDerValue] = useState(derHelpText);

    const refreshTree = (res: compiler.CompileResult) => {
        if (res.kind === 'error') {
            setTree({
                hierarchical: tree.hierarchical,
                declarations: null,
                items: [],
            });
            setDerValue(derHelpText);
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

        sortTree(items);

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
                    setDerValue(await compiler.derEncodeValue(ident));
                }
            } else {
                setDerValue(derHelpText);
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
                        getItemId={item => JSON.stringify(item, (_, value) =>
                            typeof value === 'bigint'
                                ? value.toString()
                                : value
                        )}
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
            }}>
                <Card style={{
                    padding: '10px',
                    height: '100%',
                    fontSize: '1rem',
                    wordBreak: 'break-word',
                }}>{derValue}</Card>
            </Box>
        </Box>
    );
}

export default ProjectView;
