import json
import lxml
import lxml.etree
import lxml.html
import os
import pypandoc
import re
import signal
import sys
import threading
import xlrd
import zipfile

from concurrent.futures import ThreadPoolExecutor
from module_extractor import extract_modules, NAME_REGEX
from requests_futures.sessions import FuturesSession
from typing import List
from util import make_data_dir, execute_chunked_task

SESSION = FuturesSession(executor=ThreadPoolExecutor(max_workers=os.cpu_count()))
parser = lxml.etree.HTMLParser()

def get_specs() -> List[dict]:
    wb: xlrd.Book = xlrd.open_workbook(sys.argv[1])
    sheet: xlrd.sheet.Sheet = wb.sheet_by_index(0)

    specs = []
    matches = ['protocol', 'interface']
    for (num, title, tech) in list(zip(sheet.col_values(0), sheet.col_values(2), sheet.col_values(9)))[1:]:
        lower_title = title.lower()
        if not any(match in lower_title for match in matches):
            continue
        specs.append({
            'num': num,
            'title': title,
            'tech': tech.split(','),
        })

    return specs

def download_spec_docs(spec: dict) -> List[str]:
    num = spec['num']
    num_parts = num.split('.')
    url = f'https://www.3gpp.org/ftp/Specs/archive/{num_parts[0]}_series/{num}/'

    print(f'Downloading directory listing of {url}')
    res = SESSION.get(url).result()
    if res.status_code != 200:
        print(f'got status code {res.status_code} for {url}')
        return []
    
    text = res.text
    doc = lxml.html.fromstring(text, parser=parser)

    zip_urls: List[str] = doc.xpath('//table/tbody/tr/td[2]/a/@href')
    zip_urls = [zip_url for zip_url in zip_urls if zip_url.lower().endswith('.zip')]
    if len(zip_urls) == 0:
        return []
    zip_url = zip_urls[-1]
    zip_name = zip_url[zip_url.rfind('/') + 1:]
    zip_path = os.path.join('/tmp', zip_name)

    print(f'Downloading {zip_url}')
    res = SESSION.get(zip_url).result()
    if res.status_code != 200:
        print(f'got status code {res.status_code} for {zip_url}')
        return []
    
    with open(zip_path, 'wb') as f:
        f.write(res.content)
    
    with zipfile.ZipFile(zip_path, 'r') as zip:
        zip.extractall('/tmp')

    return [f'/tmp/{file}' for file in zip.namelist() if file.endswith('.doc') or file.endswith('.docx')]

def extract_modules_from_doc(doc_path: str) -> List[str]:
    txt_path = os.path.join('/tmp', os.path.basename(doc_path) + '.txt')
    if doc_path.endswith('.doc'):
        # use antiword for old doc format
        exit_code = os.system(f'antiword {doc_path} > {txt_path}')
        if exit_code != 0:
            print(f'antiword returned {exit_code} for doc {doc_path}')
            return []
    elif doc_path.endswith('.docx'):
        # use pypandoc for modern docx format
        pypandoc.convert_file(doc_path, 'plain', outputfile=txt_path)
    else:
        print(f'unknown doc format: {doc_path}')
        return []
    
    with open(txt_path, 'r') as f:
        txt = f.read()

    print(f'extracting modules from {doc_path}')
    return extract_modules(doc_path, txt)

def download_spec_modules(data_dir: str, spec: dict) -> List[str]:
    spec_docs = download_spec_docs(spec)
    module_names = []
    for doc in spec_docs:
        modules = extract_modules_from_doc(doc)
        print(f'found {len(modules)} in {doc}')
        for module in modules:
            match = re.match(NAME_REGEX, module)
            if match is None:
                with open('/tmp/failed.asn', 'w') as f:
                    f.write(module)
                    print('NO MATCH. see /tmp/failed.asn for details')
                    os.kill(os.getpid(), signal.SIGKILL)
            module_name = match.group().strip()

            with open(os.path.join(data_dir, f'{module_name}.asn'), 'w') as f:
                f.write(module) 

            module_names.append(module_name)

    return module_names
    
mutex = threading.Lock()

def download_all_modules(data_dir: str, downloads_json_path: str, downloads: dict, specs: List[dict]):
    for spec in specs:
        if spec['num'] in downloads['specs']:
            # modules for the spec have already been downloaded; skip it
            continue

        # TODO:
        #   1. 33.128 contains two (useless) modules that should be parseable, but cause catastrophic backtracking in the regex. Unknown as to why.
        if spec['num'] in ('33.128'):
            continue

        module_names = download_spec_modules(data_dir, spec)
        if len(module_names) > 0:
            for module_name in module_names:
                print(f'Downloaded {module_name}.asn from {spec["num"]}')
                downloads['modules'].append({
                    'spec': spec['num'],
                    'module_name': module_name,
                })
        else:
            print(f'Found no ASN.1 definitions from {spec["num"]}')

        downloads['specs'].append(spec['num'])

        with mutex:
            with open(downloads_json_path, 'w') as f:
                downloads['specs'].sort()
                json.dump(downloads, f)

def main():
    if len(sys.argv) != 2 or 'help' in sys.argv[1]:
        print('usage: download-3gpp-modules.py <Specifications.xls>')
        print('download the specifications xlsx file from the 3GPP portal:')
        print('    https://portal.3gpp.org/')
        print('go to the Specifications tab, check the Technical Specifications (TS) box, and the Under Change Control box')
        print('then click Search, and then click the Download to Excel button in the top-right')
        print('then convert the downloaded file from xlsx to xls')
        print()
        exit(1)

    data_dir = make_data_dir('3GPP')

    specs = get_specs()
    print(f'Collected {len(specs)} protocol specs from XLS.')
    with open(os.path.join(data_dir, 'specs.json'), 'w') as f:
        json.dump(specs, f, indent='  ', sort_keys=True)

    modules_dir = os.path.join(data_dir, 'modules')
    if not os.path.exists(modules_dir):
        os.mkdir(modules_dir)

    downloads_json_path = os.path.join(data_dir, 'downloads.json')

    downloads: dict
    if os.path.exists(downloads_json_path):
        with open(downloads_json_path, 'r') as f:
            downloads = json.load(f)
    else:
        downloads = {
            'modules': [],
            'specs': []
        }

    execute_chunked_task(specs, lambda chunk: download_all_modules(modules_dir, downloads_json_path, downloads, chunk))

if __name__ == '__main__':
    main()
