import json
import lxml
import lxml.etree
import lxml.html
import os
import re
import signal
import threading

from concurrent.futures import ThreadPoolExecutor
from module_extractor import extract_modules, NAME_REGEX
from patches import apply_patches
from requests import Session
from requests_futures.sessions import FuturesSession
from typing import List, Tuple
from util import make_data_dir, execute_chunked_task

USE_FUTURES_SESSION = True
SESSION = FuturesSession(executor=ThreadPoolExecutor(max_workers=os.cpu_count())) if USE_FUTURES_SESSION else Session()

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
    
    return extract_modules_from_html(url, res.text)

def element_text(element) -> str:
    # all text inside the element, including the text of nested elements like <a> and <span>
    return ''.join(element.itertext())

def drop_element(element):
    # remove an element and its contents, keeping the text that follows it
    parent = element.getparent()
    tail = element.tail or ''
    previous = element.getprevious()
    if previous is not None:
        previous.tail = (previous.tail or '') + tail
    else:
        parent.text = (parent.text or '') + tail
    parent.remove(element)

def extract_modules_from_html(url: str, html: str) -> List[str]:
    # Note: RFCs converted from plain text are served as an HTML *fragment* (no <html> or <body>),
    # so all XPath queries must be relative (//) rather than anchored at /html/body.
    doc = lxml.html.fromstring(html, parser=parser)

    # Older RFCs (converted from plain text) are a sequence of <pre class="newpage"> elements,
    # one per page, each with a page header/footer in <span class="grey">.
    pages = doc.xpath('//pre[contains(concat(" ", normalize-space(@class), " "), " newpage ")]')
    if pages:
        texts = []
        for page in pages:
            # drop page headers/footers, but keep the text that follows them
            for header in page.xpath('.//span[contains(concat(" ", normalize-space(@class), " "), " grey ")]'):
                drop_element(header)
            texts.append(element_text(page).strip('\n'))
        full_text = '\n'.join(texts)
    else:
        # Newer RFCs (v3 XML format) put code in <div class="sourcecode"><pre>...</pre></div>
        # (or artwork blocks); the ASN.1 module(s) may be split across several of these.
        blocks = doc.xpath('//pre')
        full_text = '\n'.join(element_text(block).strip('\n') for block in blocks)

    return extract_modules(url, full_text)

def rfc_number(doc_id: str) -> int:
    # RFC numbers are assigned sequentially, so a higher number means a newer document
    return int(doc_id[3:])

def get_doc_modules(url: str) -> List[Tuple[str, str]]:
    # returns a list of (module name, module text)
    doc_modules = []
    for module in extract_modules_from_rfc(url):
        match = re.match(NAME_REGEX, module)
        if match is None:
            with open('/tmp/failed.asn', 'w') as f:
                f.write(module)
                print('NO MATCH. see /tmp/failed.asn for details')
                os.kill(os.getpid(), signal.SIGKILL)
        doc_modules.append((match.group().strip(), module))
    return doc_modules

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

        doc_modules = get_doc_modules(doc['url'])
        if len(doc_modules) == 0:
            print(f'Found no ASN.1 definitions from {doc["doc_id"]}')

        with mutex:
            for module_name, module in doc_modules:
                # The same module is often defined in multiple RFCs (e.g. PKIXCMP in RFC2510, RFC4210, and RFC9480),
                # and the older versions are sometimes erroneous. Only keep the version from the newest RFC.
                # Docs are processed concurrently and in no particular order, so this must be checked under the mutex.
                existing = next((m for m in downloads['modules'] if m['module_name'] == module_name), None)
                if existing is not None and rfc_number(existing['doc_id']) > rfc_number(doc['doc_id']):
                    print(f'Skipping {module_name}.asn from {doc["doc_id"]}; newer version exists in {existing["doc_id"]}')
                    continue
                if existing is not None:
                    downloads['modules'].remove(existing)

                with open(os.path.join(data_dir, f'{module_name}.asn'), 'w') as f:
                    f.write(module)

                print(f'Downloaded {module_name}.asn from {doc["doc_id"]}')
                downloads['modules'].append({
                    'doc_id': doc['doc_id'],
                    'module_name': module_name,
                })

            downloads['docs'].append(doc['doc_id'])
            downloads['docs'].sort()
            with open(downloads_json_path, 'w') as f:
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

    apply_patches('IETF', modules_dir)

if __name__ == '__main__':
    main()
