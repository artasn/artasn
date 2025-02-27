// The oids.json file is adapted from https://github.com/lapo-luchini/asn1js/blob/trunk/oids.js,
// all the data is from the original source, but:
//   1. was converted from a JavaScript file to a JSON file
//   2. had each value object converted to just the 'd' element
//   3. had the keys sorted
import Oids from './oids.json';

const oids = Oids as Record<string, string>;

export function lookupOidDescription(oid: string): string | null {
    return oids[oid] ?? null;
}
