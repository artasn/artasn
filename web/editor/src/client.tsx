import React from 'react';
import { createRoot } from 'react-dom/client';
import App from './App';
import { getWebFS } from './webfs';
import './compiler/CompileWorkerClient';

import '@fontsource/roboto/300.css';
import '@fontsource/roboto/400.css';
import '@fontsource/roboto/500.css';
import '@fontsource/roboto/700.css';

const CycleA = `
CycleA { joint-iso-itu-t baizerlabs(1337) cycle(1) module(1) a(65) } DEFINITIONS AUTOMATIC TAGS ::= BEGIN

EXPORTS ALL;

IMPORTS intermediate-id FROM CycleB { joint-iso-itu-t baizerlabs(1337) cycle(1) module(1) b(66) };

ID ::= [APPLICATION 16383] EXPLICIT OBJECT IDENTIFIER
root-id ID ::= { joint-iso-itu-t baizerlabs(1337) cycle(1) value(2) a(65) id(1) }
leaf-id ID ::= { intermediate-id leaf(420) }

END
`;

const CycleB = `
CycleB { joint-iso-itu-t baizerlabs(1337) cycle(1) module(1) b(66) } DEFINITIONS AUTOMATIC TAGS ::= BEGIN

EXPORTS ALL;

IMPORTS ID, root-id, leaf-id FROM CycleA { joint-iso-itu-t baizerlabs(1337) cycle(1) module(1) a(65) };

intermediate-id ID ::= { root-id intermediate(69) }

FileHeader ::= SEQUENCE {
    magic OCTET STRING (SIZE(8)) DEFAULT defaultMagic,
    version INTEGER,
    dataFormat ID,
    data OCTET STRING
}

defaultMagic OCTET STRING ::= '41534E3143484546'H
currentVersion INTEGER ::= 3

simpleHeader FileHeader ::= {
	magic defaultMagic,
	version currentVersion,
	dataFormat leaf-id,
	data '48656C6C6F2C20776F726C6421'H
}

END
`;

(async () => {
    const textEncoder = new TextEncoder();

    const fs = await getWebFS();
    if (!fs.exists('/code')) {
        await fs.mkdir('/code');
        await fs.writeFile('/code/CycleA.asn', textEncoder.encode(CycleA), { create: true, overwrite: true });
        await fs.writeFile('/code/CycleB.asn', textEncoder.encode(CycleB), { create: true, overwrite: true });
    }
})();

const root = createRoot(document.getElementById('root')!);
root.render(<App />);
