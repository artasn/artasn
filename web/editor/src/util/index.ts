export * from './ObjectSet';
export * from './react-util';

export function stringifyJSON(json: any): string {
    return JSON.stringify(json, (_, value) =>
        typeof value === 'bigint'
            ? value.toString()
            : value
    );
}

