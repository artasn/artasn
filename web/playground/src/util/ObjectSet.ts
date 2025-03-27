export type KeyFunc<T> = keyof T | ((object: T) => string);

export default class ObjectSet<T extends Record<string, any>> {
    private elements: Record<string, T> = {};

    public constructor(private keyFunc: KeyFunc<T>, elements?: Iterable<T>) {
        if (elements) {
            for (const element of elements) {
                this.add(element);
            }
        }
    }

    private getKey(object: T): string {
        if (typeof this.keyFunc === 'function') {
            return this.keyFunc(object);
        } else {
            return this.keyFunc as string;
        }
    }

    public get size(): number {
        return Object.keys(this.elements).length;
    }

    public add(...elements: T[]) {
        for (const element of elements) {
            this.elements[this.getKey(element)] = element;
        }
    }

    public delete(element: T) {
        delete this.elements[this.getKey(element)];
    }

    public has(element: T) {
        return this.getKey(element) in this.elements;
    }

    public array(): T[] {
        return Object.values(this.elements);
    }

    public sortedArray(): T[] {
        const array = this.array();
        array.sort((a, b) => this.getKey(a).localeCompare(this.getKey(b)));
        return array;
    }

    *[Symbol.iterator]() {
        for (const element of this.array()) {
            yield element;
        }
    }
}
