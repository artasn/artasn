import type { AstElement, AstItem } from 'asn1-extension-protocol';

export function findAstItems<T extends AstItem>(root: AstItem, itemKind: T['ast']): T[] {
    const items: T[] = [];
    findAstItemsInternal(root, itemKind, items);
    return items;
}

function findAstItemsInternal(root: AstItem, itemKind: AstItem['ast'], items: AstItem[]) {
    if (root.ast === itemKind) {
        items.push(root);
    }

    for (const key of Object.keys(root)) {
        const value = (root as any)[key];
        if (value === null || typeof value !== 'object') {
            continue;
        }
        if (Array.isArray(value)) {
            const arr = value as AstElement<AstItem>[];
            for (const item of arr) {
                findAstItemsInternal(item.element, itemKind, items);
            }
        } else if ('element' in value && 'loc' in value) {
            const item = value as AstElement<AstItem>;
            findAstItemsInternal(item.element, itemKind, items);
        }
    }
}
