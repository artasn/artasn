import hashlib
import json
import os
import shutil

from libdeps import get_module_dependencies
from typing import Dict, List, Set, Tuple
from util import make_data_dir, get_data_dir

ietf_skip_modules = set([
    # contains empty OIDs (i.e. '{}') and the IMPORTS section doesn't end with a semicolon
    'ETS-ElectronicSignatureFormats-88syntax.asn',
    # module name is a valuereference and therefore invalid
    'scrypt-0.asn',
    # contains a comma before the FROM keyword
    'PasswordRecipientInfo-97.asn',
    # contains a TagDefault as just 'TAGS', without 'EXPLICIT', 'IMPLICIT', or 'AUTOMATIC', which violates the X.680 spec
    'ModuleNumbers.asn',
    # false positive from module_extractor
    'ESROS.asn',
    # IMPORT statements contain no symbols, just module names
    'FourBSD-ISODE.asn',
    # IMPORTS section doesn't end with a semicolon
    'DPI20-MIB.asn',
    # missing comma at end of line 62
    'CMSECCAlgs-2009-88.asn',
    # IMPORTS section doesn't end with a semicolon
    'POSTSCRIPT-MAPPINGS.asn',
    # missing FROM before 'PKIX1Explicit-2009'
    'ClearanceSponsorAttribute-2008.asn',
    # comment on line 12 has one hyphen instead of two
    'ETS-ElectronicSignaturePolicies-88syntax.asn',
    # contains a comma before the FROM keyword
    'SONET-MIB.asn',
    # IMPORTS section doesn't end with a semicolon
    'PKIXSIM.asn',
    # false positive from module_extractor
    'Steve.asn',
    # root node of module oid is 'tbd1' which is invalid
    'ISO10589-ISIS.asn',
    # comment on line 37 prefixed with '- -' instead of '--'
    'ETS-ElectronicSignatureFormats-97Syntax.asn',
    # module name is a valuereference and therefore invalid
    'mod-SMimeSecureHeadersV1.asn',
    # contains a comma before the FROM keyword
    'SNMP-PROXY-MIB.asn',
])

itu_t_skip_modules = set([])

skip_modules = ietf_skip_modules.union(itu_t_skip_modules)

def get_all_dependencies(dir: str) -> List[dict]:
    i = 0
    for root, _, files in os.walk(dir):
        for file in files:
            if os.path.basename(file) in skip_modules:
                continue
            file = os.path.join(root, file)
            with open(file, 'r') as f:
                i += 1
                module_source = f.read()
                print(f'file = {file} (#{i})')
                # TODO: make modules imported with oid by valuereference as having unknown version
                deps = get_module_dependencies(module_source)
                print(f'deps = {deps}')
                print('------------------')

def get_ietf_collections(docs: List[dict], modules: List[dict]) -> List[dict]:
    # set that contains all docs that are updates to other docs
    update_docs: Set[str] = set()
    for doc in docs:
        for updated_by in doc['updated_by']:
            update_docs.add(updated_by)

    # list that contains all docs that are not updates to other docs ("root docs")
    root_docs: List[dict] = []
    for doc in docs:
        if doc['doc_id'] not in update_docs:
            root_docs.append(doc)

    # list containing tuple of (each root doc, each update that doc has)
    docs_with_updates: List[Tuple[dict, List[dict]]] = []
    for root_doc in root_docs:
        update_ids = root_doc['updated_by']
        if len(update_ids) == 0:
            updates = []
        else:
            updates = list(map(lambda update_id: next(doc for doc in docs if doc['doc_id'] == update_id), update_ids))
        docs_with_updates.append((root_doc, updates))

    collections = []
    for (root_doc, update_ids) in docs_with_updates:
        doc_modules = set()
        update_modules = []

        # make updates a set as an optimization
        updates_set = set(map(lambda update: update['doc_id'], update_ids))

        for module in modules:
            file_path = f'modules/{module["module_name"]}.asn'
            module_doc = module['doc_id']
            if module_doc == root_doc['doc_id']:
                doc_modules.add(file_path)
            elif module_doc in updates_set:
                update_doc = None
                for update_module in update_modules:
                    if update_module['name'] == module_doc:
                        update_doc = update_module
                        break

                if update_doc is None:
                    doc = next(update for update in update_ids if update['doc_id'] == module_doc)
                    update_doc = {
                        'name': doc['doc_id'],
                        'title': doc['title'],
                        'url': doc['url'],
                        'modules': set(),
                    }
                    update_modules.append(update_doc)

                update_doc['modules'].add(file_path)

        if len(doc_modules) > 0:
            collection = {
                'name': root_doc['doc_id'],
                'title': root_doc['title'],
                'url': root_doc['url'],
                'modules': list(doc_modules),
            }
            if len(update_modules) > 0:
                for update_module in update_modules:
                    update_module['modules'] = list(update_module['modules'])
                collection['updates'] = update_modules
            collections.append(collection)

    return collections

def create_ietf_registry(registry_dir: str) -> bytes:
    ietf_data_dir = get_data_dir('IETF')
    with open(os.path.join(ietf_data_dir, 'index.json'), 'r') as f:
        index = json.load(f)
    with open(os.path.join(ietf_data_dir, 'downloads.json'), 'r') as f:
        downloads = json.load(f)

    modules_dir = os.path.join(registry_dir, 'modules')
    shutil.copytree(os.path.join(ietf_data_dir, 'modules'), modules_dir, dirs_exist_ok=True)
    deps = get_all_dependencies(modules_dir)
    print(deps)

    collections = get_ietf_collections(index, downloads['modules'])
    collections_json = json.dumps(collections, sort_keys=True)
    with open(os.path.join(registry_dir, 'registry.json'), 'w') as f:
        f.write(collections_json)
        hash = hashlib.sha256()
        hash.update(collections_json.encode())
        return hash.digest()

def get_itu_t_collections(recs: List[dict], modules: List[dict]) -> List[dict]:
    # fast lookup for recommendations
    rec_lookup: Dict[str, dict] = {}
    for rec in recs:
        rec_lookup[rec['name']] = rec

    # mapping of each rec name -> the modules in that rec
    modules_by_rec: Dict[str, List[dict]] = {}
    for module in modules:
        rec = rec_lookup[module['rec']]
        rec_name = rec['name']
        if rec_name not in modules_by_rec:
            modules_by_rec[rec_name] = []

        rec_modules = modules_by_rec[rec_name]
        rec_modules.append(module)

    collections = []
    for (rec_name, modules) in modules_by_rec.items():
        rec = rec_lookup[rec_name]
        collections.append({
            'name': rec['name'],
            'approval': rec['approval'],
            'title': rec['title'].replace('\u2013', '-'),
            'modules': list(map(lambda module: f'modules/{rec["name"]}/{module["name"]}.asn', modules)),
        })

    return collections

def create_itu_t_registry(registry_dir: str) -> bytes:
    itu_t_data_dir = get_data_dir('ITU-T')
    with open(os.path.join(itu_t_data_dir, 'recommendations.json'), 'r') as f:
        recommendations = json.load(f)
    with open(os.path.join(itu_t_data_dir, 'modules.json'), 'r') as f:
        modules = json.load(f)

    modules_dir = os.path.join(registry_dir, 'modules')
    shutil.copytree(os.path.join(itu_t_data_dir, 'modules'), modules_dir, dirs_exist_ok=True)
    deps = get_all_dependencies(modules_dir)
    print(deps)

    collections = get_itu_t_collections(recommendations, modules)
    collections_json = json.dumps(collections, sort_keys=True)
    with open(os.path.join(registry_dir, 'registry.json'), 'w') as f:
        f.write(collections_json)
        hash = hashlib.sha256()
        hash.update(collections_json.encode())
        return hash.digest()


def main():
    data_dir = make_data_dir('package-registry')
    registry_dir = os.path.join(data_dir, 'registry')

    if not os.path.exists(registry_dir):
        os.mkdir(registry_dir)

    ietf_dir = os.path.join(registry_dir, 'IETF')
    if not os.path.exists(ietf_dir):
        os.mkdir(ietf_dir)
    ietf_hash = create_ietf_registry(ietf_dir)

    itu_t_dir = os.path.join(registry_dir, 'ITU-T')
    if not os.path.exists(itu_t_dir):
        os.mkdir(itu_t_dir)
    itu_t_hash = create_itu_t_registry(itu_t_dir)

    hash = hashlib.sha256()
    hash.update(ietf_hash)
    hash.update(itu_t_hash)
    registry_hash = hash.hexdigest()

    sources = {
        'hash': registry_hash,
        'sources': [
            {
                'hash': ietf_hash.hex(),
                'name': 'IETF',
                'fullName': 'Internet Engineering Task Force',
                'desc': 'IETF is responsible for the technical standards (called RFCs) that make up the Internet protocol suite, such as PKIX for public-key infrastructure, PKCS for public-key cryptography, and many others.',
                'registry': 'registry/IETF/registry.json',
            },
            {  
                'hash': itu_t_hash.hex(),
                'name': 'ITU-T',
                'fullName': 'International Telecommunication Union Telecommunication Standardization Sector',
                'desc': 'ITU-T is responsible for coordinating standards such as X.509 for cybersecurity, H.264 for video compression, and many others.',
                'registry': 'registry/ITU-T/registry.json'
            }
        ],
    }

    with open(os.path.join(data_dir, 'hash.txt'), 'w') as f:
        f.write(registry_hash)

    with open(os.path.join(data_dir, 'sources.json'), 'w') as f:
        json.dump(sources, f)

if __name__ == '__main__':
    main()
