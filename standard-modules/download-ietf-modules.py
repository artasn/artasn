import json
import lxml
import lxml.etree
import lxml.html
import os
import re
import signal
import threading

from concurrent.futures import ThreadPoolExecutor
from requests import Session
from requests_futures.sessions import FuturesSession
from typing import List
from util import make_data_dir, execute_chunked_task

USE_FUTURES_SESSION = True
SESSION = FuturesSession(executor=ThreadPoolExecutor(max_workers=os.cpu_count())) if USE_FUTURES_SESSION else Session()

BEGIN_MODULE_REGEX = re.compile(r'[^\-]\bDEFINITIONS\b.*?::=.*?\bBEGIN\b', re.S)
BEGIN_REGEX = re.compile(r'\bBEGIN\b')
END_MODULE_REGEX = re.compile(r'.+\bEND\b', re.S)
END_REGEX = re.compile(r'\bEND\b')
MACRO_REGEX = re.compile(r'\bMACRO\b')
NAME_REGEX = re.compile(r'\s*\b[A-Za-z][A-Za-z0-9\-]*\b')

MODULE_REGEX_BASE = '([A-Za-z][A-Za-z0-9\-]*)\s*({\s*([A-Za-z0-9\-]+(\s*\([0-9]+\))?\s*)+})?(\s*--.*?\n)*\s*DEFINITIONS\s*?([A-Z\s]+)?\s*?::=\s*?([A-Z\s]+)?\s*?BEGIN'
MODULE_REGEX = re.compile(fr'^\s*{MODULE_REGEX_BASE}', re.S)
FAST_MODULE_REGEX = re.compile(MODULE_REGEX_BASE, re.S)

MACRO_STATE_DEFINITION = 0
MACRO_STATE_BEGIN = 1
MACRO_STATE_END = 2

parser = lxml.etree.HTMLParser()
XML_NS = 'https://www.rfc-editor.org/rfc-index'

def download_index_xml(data_dir: str) -> bytes:
    index_path = os.path.join(data_dir, 'index.xml')
    if not os.path.exists(index_path):
        res = SESSION.get('https://www.rfc-editor.org/rfc-index.xml')
        if USE_FUTURES_SESSION:
            res = res.result()
        text = res.text
        with open(index_path, 'w') as f:
            f.write(text)

    with open(index_path, 'rb') as f:
        return f.read()
    
def tag(name: str) -> lxml.etree.QName:
    return lxml.etree.QName(XML_NS, name)
    
def parse_index_xml(index: bytes) -> List[dict]:
    docs = []

    doc = lxml.etree.fromstring(index)
    for rfc_entry in doc.findall(tag('rfc-entry')):
        doc_id: str = rfc_entry.find(f'{tag("doc-id")}').text
        title: str = rfc_entry.find(f'{tag("title")}').text
        url = f'https://www.rfc-editor.org/rfc/rfc{doc_id.lower()[3:].lstrip("0")}.html'

        updated_by = []
        ub = rfc_entry.find(f'{tag("updated-by")}')
        if ub is not None and len(ub) > 0:
            for update_entry in ub.getchildren():
                updated_by.append(update_entry.text)

        docs.append({
            'doc_id': doc_id,
            'title': title,
            'url': url,
            'updated_by': updated_by,
        })

    return docs

def extract_modules_from_rfc(url: str) -> List[str]:
    print(f'extracting from {url}')

    res = SESSION.get(url)
    if USE_FUTURES_SESSION:
        res = res.result()
    if res.status_code != 200:
        print(f'got status code {res.status_code} for {url}')
        return []
    
    print(f'downloaded {url}')
    
    text = res.text
    doc = lxml.html.fromstring(text, parser=parser)

    print(f'parsed {url}')

    # replace links with their inner text
    for link in doc.xpath('//pre[@class="newpage"]//a'):
        text = link.text
        previous = link.getprevious()
        if previous is not None:
            previous.tail = ((previous.tail or '') + text + (link.tail or '')).rstrip('\n')
            # previous.tail = text
            link.getparent().remove(link)

    # newer RFCs have a different HTML format
    new_format_code = doc.xpath('//div[@id="asn1-module"]/section/div/pre/text()')
    if new_format_code:
        code = new_format_code[0].lstrip()
        code_start = re.search(FAST_MODULE_REGEX, code)
        code_end = re.search(END_MODULE_REGEX, code) # END_MODULE_REGEX picks the last END, such that all MACRO definitions are included
        if code_start is not None:
            code_start = code_start.start()
        else:
            print(f'failed to match MODULE_REGEX for FAST doc {url}')
            os.kill(os.getpid(), signal.SIGKILL)
        if code_end is not None:
            code_end = code_end.end()
        else:
            print(f'failed to match END_MODULE_REGEX for FAST doc {url}')
            os.kill(os.getpid(), signal.SIGKILL)
        return [code[code_start:code_end] + '\n']

    text_nodes: List[str] = doc.xpath('/html/body/pre[@class="newpage"]/text()')
    full_text = '\n'.join(map(lambda text_node: text_node.strip('\n'), text_nodes))
    modules = []

    def append_module(module: str):
        lines = module.split('\n')
        common_space = None
        for line in lines:
            if len(line) > 0 and line[0] != ' ':
                common_space = None
                break

            for i in range(len(line)):
                if line[i] != ' ':
                    if common_space is None:
                        common_space = i
                    else:
                        common_space = min(common_space, i)
                    break

        if common_space is not None:
            for i in range(len(lines)):
                lines[i] = lines[i][common_space:]

        modules.append('\n'.join(lines) + '\n')

    while True:
        if 'DEFINITIONS' in full_text and 'BEGIN' in full_text:
            begin = re.search(BEGIN_MODULE_REGEX, full_text)
            if begin is not None:
                begin_end = begin.end()
            else:
                break

            cur_line_index = full_text.rfind('\n', 0, begin_end)

            # find location of BEGIN, then check every previous line until MODULE_REGEX matches

            def next_line():
                nonlocal url
                nonlocal cur_line_index
                cur_line_index = full_text.rfind('\n', 0, cur_line_index)
                if cur_line_index == -1:
                    print(f'failed to match MODULE_REGEX for doc {url}')
                    return False
                return True

            while True:
                header_text = full_text[cur_line_index:begin_end]
                if header_text.lstrip().startswith('--'):
                    # skip comments
                    if not next_line():
                        return modules

                match = re.search(MODULE_REGEX, header_text)
                if match:
                    # make full_text start at the matched line
                    full_text = full_text[cur_line_index+1:]

                    # find the first END that isn't part of a MACRO definition
                    macro_state = MACRO_STATE_DEFINITION
                    mod_line_index = full_text.find('\n')

                    module_end_index = None
                    while mod_line_index < len(full_text):
                        next_line_index = full_text.find('\n', mod_line_index+1)
                        if next_line_index == -1:
                            next_line_index = len(full_text)
                        line = full_text[mod_line_index+1:next_line_index]
                        mod_line_index = next_line_index

                        if macro_state == MACRO_STATE_DEFINITION:
                            if 'MACRO' in line and re.search(MACRO_REGEX, line) is not None:
                                macro_state = MACRO_STATE_BEGIN
                            elif 'END' in line and re.search(END_REGEX, line) is not None:
                                module_end_index = mod_line_index
                                break
                        if macro_state == MACRO_STATE_BEGIN:
                            if 'BEGIN' in line and re.search(BEGIN_REGEX, line) is not None:
                                macro_state = MACRO_STATE_END
                        
                        if macro_state == MACRO_STATE_END:
                            if 'END' in line and re.search(END_REGEX, line) is not None:
                                macro_state = MACRO_STATE_DEFINITION

                    if not module_end_index:
                        print(f'warning: module without END from doc {url}')
                        return modules
                    
                    module_text = full_text[:module_end_index]
                    append_module(module_text)

                    full_text = full_text[module_end_index:]

                    # find more modules
                    break
                else:
                    if not next_line():
                        return modules
        else:
            # no more module definitions in the doc; exit
            break

    return modules

def download_doc_modules(data_dir: str, url: str) -> List[str]:
    modules = extract_modules_from_rfc(url)
    if len(modules) > 0:
        module_names = []
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
    else:
        return []

mutex = threading.Lock()

def download_all_modules(data_dir: str, downloads_json_path: str, downloads: dict, docs: List[dict]):
    for doc in docs:
        if doc['doc_id'] in downloads['docs']:
            # modules for the doc have already been downloaded; skip it
            continue

        # TODO:
        #   1. RFC1024 should be ignored, but contains the words and 'DEFINITIONS' and 'BEGIN' but not in module format
        #   2. RFC1224 causes catastrophic backtracking for MODULE_REGEX, unknown reason why; contains a valid module
        if doc['doc_id'] in ('RFC1024', 'RFC1224'):
            continue

        # if doc['doc_id'] not in ('RFC5208'):
        #     continue

        module_names = download_doc_modules(data_dir, doc['url'])
        if len(module_names) > 0:
            for module_name in module_names:
                print(f'Downloaded {module_name}.asn from {doc["doc_id"]}')
                downloads['modules'].append({
                    'doc_id': doc['doc_id'],
                    'module_name': module_name,
                })
        else:
            print(f'Found no ASN.1 definitions from {doc["doc_id"]}')

        downloads['docs'].append(doc['doc_id'])

        with mutex:
            with open(downloads_json_path, 'w') as f:
                downloads['docs'].sort()
                json.dump(downloads, f)

def main():
    data_dir = make_data_dir('IETF')

    index = download_index_xml(data_dir)
    docs = parse_index_xml(index)
    with open(os.path.join(data_dir, 'index.json'), 'w') as f:
        json.dump(docs, f, indent='  ')

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
            'docs': []
        }

    execute_chunked_task(docs, lambda chunk: download_all_modules(modules_dir, downloads_json_path, downloads, chunk))

if __name__ == '__main__':
    main()
