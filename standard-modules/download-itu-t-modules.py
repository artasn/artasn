import json
import lxml
import lxml.html
import os
import requests
import sys

from typing import List, NamedTuple, Optional
from patches import apply_patches
from util import make_data_dir, execute_chunked_task

class Cell(NamedTuple):
    text: str
    href: Optional[str]

def read_export_table(path: str) -> List[List[Cell]]:
    # The ITU-T website's "Export result to MS Excel file" button doesn't produce a real
    # spreadsheet: it serves the HTML results table with a .xls extension (which Excel can open).
    # Parse that HTML table directly, returning the data rows (excluding the header row).
    doc = lxml.html.parse(path).getroot()
    tables = doc.xpath('//table[@id="ctl00_content_result_grd_result"]')
    if len(tables) != 1:
        raise ValueError(f'{path} does not look like an ITU-T export: could not find the results table')

    rows = []
    for tr in tables[0].xpath('./tr[td]'):
        row = []
        for td in tr.xpath('./td'):
            links = td.xpath('.//a/@href')
            row.append(Cell(td.text_content().strip(), links[0] if links else None))
        rows.append(row)

    return rows

def get_recommendations() -> List[dict]:
    recommendations = []
    for row in read_export_table(sys.argv[1]):
        rec = row[0].text
        title = row[1].text
        paren_index = rec.rindex('(')
        name = rec[:paren_index].strip()
        approval = rec[paren_index:][1:-1]

        recommendations.append({
            'name': name,
            'approval': approval,
            'title': title,
        })

    return recommendations

def get_modules(recommendations: dict) -> List[dict]:
    modules = []
    for row in read_export_table(sys.argv[2]):
        rec_name = row[0].text
        rec_opts = [rec for rec in recommendations if rec['name'] == rec_name]
        if len(rec_opts) == 0:
            continue
        if len(rec_opts) > 1:
            raise ValueError(f'too many matching recommendations: {rec_opts}')
        recommendation = rec_opts[0]

        approval_parts = row[1].text.split('-')
        approval = f'{approval_parts[1]}/{approval_parts[0]}'
        if recommendation['approval'] != approval:
            continue

        module_name = row[3].text
        hyperlink = row[3].href or 'unknown'

        modules.append({
            'rec': rec_name,
            'name': module_name,
            'source': hyperlink,
        })

    return modules

def download_modules(modules_dir: str, modules: List[dict]):
    for module in modules:
        module_id = f'{module["rec"]}/{module["name"]}'
        if module['source'] == 'unknown':
            print(f'Skipping module {module_id}')
            continue

        download_path = os.path.join(modules_dir, module['rec'], module['name'] + '.asn')
        download_dir = os.path.dirname(download_path)
        if not os.path.exists(download_dir):
            os.mkdir(download_dir)

        text = requests.get(module['source']).text
        print(f'Downloaded {module_id}')

        text = text.replace('<br>', '\n')
        doc = lxml.html.fromstring(text)
        text_nodes: List[str] = doc.xpath('/html/body/pre//text()')
        content = ''.join(text_nodes).strip() + '\n'

        with open(download_path, 'w') as f:
            f.write(content)

def main():
    if len(sys.argv) != 3 or 'help' in sys.argv[1]:
        print('usage: download-itu-t-modules.py <Recommendations.xls> <Modules.xls>')
        print('download the "Export result to MS Excel file" files from the ITU-T website:')
        print('    Recommendations: https://www.itu.int/ITU-T/recommendations/search.aspx?type=30&status=F&main=1&pg_size=100')
        print('    Modules:         https://www.itu.int/ITU-T/recommendations/fl.aspx?lang=1&pg_size=100')
        print()
        exit(1)

    data_dir = make_data_dir('ITU-T')

    recommendations = get_recommendations()
    print(f'Collected {len(recommendations)} recommendations from export.')
    with open(os.path.join(data_dir, 'recommendations.json'), 'w') as f:
        json.dump(recommendations, f, indent='  ', sort_keys=True)

    modules = get_modules(recommendations)
    print(f'Collected {len(modules)} modules from export.')
    with open(os.path.join(data_dir, 'modules.json'), 'w') as f:
        json.dump(modules, f, indent='  ', sort_keys=True)

    modules_dir = os.path.join(data_dir, 'modules')
    if not os.path.exists(modules_dir):
        os.mkdir(modules_dir)

    execute_chunked_task(modules, lambda chunk: download_modules(modules_dir, chunk))

    apply_patches('ITU-T', modules_dir)

if __name__ == '__main__':
    main()
