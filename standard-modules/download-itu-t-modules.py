import itertools
import json
import lxml
import lxml.html
import os
import requests
import sys
import xlrd

from typing import List
from util import make_data_dir, execute_chunked_task

def get_recommendations() -> List[dict]:
    wb: xlrd.Book = xlrd.open_workbook(sys.argv[1])
    sheet: xlrd.sheet.Sheet = wb.sheet_by_index(0)

    recommendations = []
    for (rec_cell, title_cell) in list(zip(sheet.col_slice(0), sheet.col_slice(1)))[4:]:
        rec: str = rec_cell.value
        title: str = title_cell.value
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
    wb: xlrd.Book = xlrd.open_workbook(sys.argv[2])
    sheet: xlrd.sheet.Sheet = wb.sheet_by_index(0)

    modules = []
    for (rec_cell, approval_cell, row) in list(zip(sheet.col_slice(0), sheet.col_slice(1), itertools.count(0)))[4:]:
        rec_name: str = rec_cell.value
        rec_opts = [rec for rec in recommendations if rec['name'] == rec_name]
        if len(rec_opts) == 0:
            continue
        if len(rec_opts) > 1:
            raise ValueError(f'too many matching recommendations: {rec_opts}')
        recommendation = rec_opts[0]

        approval: str = approval_cell.value
        approval_parts = approval.split('-')
        approval = f'{approval_parts[1]}/{approval_parts[0]}'
        if recommendation['approval'] != approval:
            continue

        module_name: str = sheet.cell_value(row, 3)
        pos = (row, 3)
        if pos in sheet.hyperlink_map:
            hyperlink = sheet.hyperlink_map[pos].url_or_path
        else:
            hyperlink = 'unknown'

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
        print('download the xls files from the ITU-T website:')
        print('    Recommendations: https://www.itu.int/ITU-T/recommendations/search.aspx?type=30&status=F&main=1&pg_size=100')
        print('    Modules:         https://www.itu.int/ITU-T/recommendations/fl.aspx?lang=1&pg_size=100')
        print()
        exit(1)

    data_dir = make_data_dir('ITU-T')

    recommendations = get_recommendations()
    print(f'Collected {len(recommendations)} recommendations from XLS.')
    with open(os.path.join(data_dir, 'recommendations.json'), 'w') as f:
        json.dump(recommendations, f, indent='  ', sort_keys=True)

    modules = get_modules(recommendations)
    print(f'Collected {len(modules)} modules from XLS.')
    with open(os.path.join(data_dir, 'modules.json'), 'w') as f:
        json.dump(modules, f, indent='  ', sort_keys=True)

    modules_dir = os.path.join(data_dir, 'modules')
    if not os.path.exists(modules_dir):
        os.mkdir(modules_dir)

    execute_chunked_task(modules, lambda chunk: download_modules(modules_dir, chunk))

if __name__ == '__main__':
    main()
