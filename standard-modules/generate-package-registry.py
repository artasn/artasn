import gzip
import hashlib
import json
import libdeps
import os
import shutil

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

itu_t_skip_modules = set([
    # missing comma at end of line 11
    'Wrapper.asn',
    # contains 'IMPLICIT TAGS' twice
    'ProtProtocols.asn',
    # empty module
    # TODO: the actual module here, 'T38(2002).asn' should just be renamed to 'T38.asn'
    'T38(1998).asn',
    # not an actual module
    'XmppIdentity.asn',
    # missing closing '}' for oid on line 79
    'DirectoryAbstractService.asn',
])

skip_modules = ietf_skip_modules.union(itu_t_skip_modules)

def get_all_dependencies(dir: str) -> List[dict]:
    module_deps = []
    for root, _, files in os.walk(dir):
        for file in files:
            if os.path.basename(file) in skip_modules:
                continue
            file = os.path.join(root, file)
            with open(file, 'r') as f:
                module_source = f.read()
                # TODO: make modules imported with oid by valuereference as having unknown version
                # or, even better, resolve the actual version
                module_deps.append(libdeps.get_module_dependencies(module_source))
    return module_deps

def get_module_dependencies(module: dict, dependencies: List[dict]):
    module_dependencies = module.get('dependencies', [])
    if len(module_dependencies) > 0:
        dependencies.extend(module_dependencies)
    module_updates = module.get('updates', [])
    if len(module_updates) > 0:
        for module_update in module_updates:
            get_module_dependencies(module_update, dependencies)

def insert_package_dependencies(registries: List[Tuple[str, dict]]):
    for _, registry in registries:
        for package in registry:
            dependencies = []
            for module in package['modules']:
                get_module_dependencies(module, dependencies)

            packages = {}
            for other_registry_name, other_registry in registries:
                for other_package in other_registry:
                    if other_package['name'] == package['name']:
                        continue
                    for module in other_package['modules']:
                        for dependency in dependencies:
                            if module.get('oid', None) == dependency['oid'] and module['name'] == dependency['name']:
                                packages[other_package['name']] = {
                                    'source': other_registry_name,
                                    'name': other_package['name'],
                                }

            if len(packages) > 0:
                package['dependencies'] = list(packages.values())

def create_module_json(path: str, name: str, dep: dict) -> dict:
    obj = {
        'path': path,
        'name': name,
    }
    if dep is not None:
        obj['oid'] = dep['module']['oid']
        if len(dependencies := dep['dependencies']) > 0:
            obj['dependencies'] = dependencies
    return obj

def ietf_module_to_json(module: dict, module_deps: List[dict]) -> dict:
    dep = next((module_dep for module_dep in module_deps if module_dep['module']['name'] == module['module_name']), None)
    return create_module_json(f'modules/{module["module_name"]}.asn', module['module_name'], dep)

def get_ietf_registry(docs: List[dict], modules: List[dict], module_deps: List[dict]) -> List[dict]:
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
        doc_modules = {}
        update_modules = []

        # make updates a set as an optimization
        updates_set = set(map(lambda update: update['doc_id'], update_ids))

        for module in modules:
            module_doc = module['doc_id']
            if module_doc == root_doc['doc_id']:
                obj = ietf_module_to_json(module, module_deps)
                doc_modules[obj['path']] = obj
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
                        'modules': {},
                    }
                    update_modules.append(update_doc)

                obj = ietf_module_to_json(module, module_deps)
                update_doc['modules'][obj['path']] = obj

        if len(doc_modules) > 0:
            collection = {
                'name': root_doc['doc_id'],
                'title': root_doc['title'],
                'url': root_doc['url'],
                'modules': list(doc_modules.values()),
            }
            if len(update_modules) > 0:
                for update_module in update_modules:
                    update_module['modules'] = list(update_module['modules'].values())
                collection['updates'] = update_modules
            collections.append(collection)

    return collections

def create_ietf_registry(registry_dir: str) -> List[dict]:
    ietf_data_dir = get_data_dir('IETF')
    with open(os.path.join(ietf_data_dir, 'index.json'), 'r') as f:
        index = json.load(f)
    with open(os.path.join(ietf_data_dir, 'downloads.json'), 'r') as f:
        downloads = json.load(f)

    modules_dir = os.path.join(registry_dir, 'modules')
    shutil.copytree(os.path.join(ietf_data_dir, 'modules'), modules_dir, dirs_exist_ok=True)
    module_deps = get_all_dependencies(modules_dir)

    return get_ietf_registry(index, downloads['modules'], module_deps)

def itu_t_module_to_json(rec: dict, module: dict, module_deps: List[dict]) -> dict:
    dep = next((module_dep for module_dep in module_deps if module_dep['module']['name'] == module['name']), None)
    return create_module_json(f'modules/{rec["name"]}/{module["name"]}.asn', module['name'], dep)

def get_itu_t_registry(recs: List[dict], modules: List[dict], module_deps: List[dict]) -> List[dict]:
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
            'modules': list(map(lambda module: itu_t_module_to_json(rec, module, module_deps), modules)),
        })

    return collections

def create_itu_t_registry(registry_dir: str) -> List[dict]:
    itu_t_data_dir = get_data_dir('ITU-T')
    with open(os.path.join(itu_t_data_dir, 'recommendations.json'), 'r') as f:
        recommendations = json.load(f)
    with open(os.path.join(itu_t_data_dir, 'modules.json'), 'r') as f:
        modules = json.load(f)

    modules_dir = os.path.join(registry_dir, 'modules')
    shutil.copytree(os.path.join(itu_t_data_dir, 'modules'), modules_dir, dirs_exist_ok=True)
    module_deps = get_all_dependencies(modules_dir)

    return get_itu_t_registry(recommendations, modules, module_deps)

def save_registry(registry_dir: str, registry: List[dict]) -> bytes:
    registry_json = json.dumps(registry, sort_keys=True)

    with gzip.open(os.path.join(registry_dir, 'registry.json.gz'), 'wb') as f:
        f.write(registry_json.encode())

    with open(os.path.join(registry_dir, 'registry.json'), 'w') as f:
        f.write(registry_json)
        hash = hashlib.sha256()
        hash.update(registry_json.encode())
        return hash.digest()

def main():
    data_dir = make_data_dir('package-registry')
    registry_dir = os.path.join(data_dir, 'registry')

    if not os.path.exists(registry_dir):
        os.mkdir(registry_dir)

    ietf_dir = os.path.join(registry_dir, 'IETF')
    if not os.path.exists(ietf_dir):
        os.mkdir(ietf_dir)
    ietf_registry = create_ietf_registry(ietf_dir)

    itu_t_dir = os.path.join(registry_dir, 'ITU-T')
    if not os.path.exists(itu_t_dir):
        os.mkdir(itu_t_dir)
    itu_t_registry = create_itu_t_registry(itu_t_dir)

    insert_package_dependencies([('IETF', ietf_registry), ('ITU-T', itu_t_registry)])

    ietf_hash = save_registry(ietf_dir, ietf_registry)
    itu_t_hash = save_registry(itu_t_dir, itu_t_registry)

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
                'registry': {
                    'raw': 'registry/IETF/registry.json',
                    'gzip': 'registry/IETF/registry.json.gz',
                },
            },
            {  
                'hash': itu_t_hash.hex(),
                'name': 'ITU-T',
                'fullName': 'International Telecommunication Union Telecommunication Standardization Sector',
                'desc': 'ITU-T is responsible for coordinating standards such as X.509 for cybersecurity, H.264 for video compression, and many others.',
                'registry': {
                    'raw': 'registry/ITU-T/registry.json',
                    'gzip': 'registry/ITU-T/registry.json.gz'
                },
            }
        ],
    }

    with open(os.path.join(data_dir, 'hash.txt'), 'w') as f:
        f.write(registry_hash)

    with open(os.path.join(data_dir, 'sources.json'), 'w') as f:
        json.dump(sources, f)

if __name__ == '__main__':
    main()
