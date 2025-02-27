export * from './ObjectSet';

export function stringifyJSON(json: any): string {
    return JSON.stringify(json, (_, value) =>
        typeof value === 'bigint'
            ? value.toString()
            : value
    );
}
