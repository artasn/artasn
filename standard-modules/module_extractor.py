import re
from typing import List

BEGIN_MODULE_REGEX = re.compile(r'[^\-]\bDEFINITIONS\b.*?::=.*?\bBEGIN\b', re.S)
BEGIN_REGEX = re.compile(r'\bBEGIN\b')
END_REGEX = re.compile(r'\bEND\b')
MACRO_REGEX = re.compile(r'\bMACRO\b')
NAME_REGEX = re.compile(r'\s*\b[A-Za-z][A-Za-z0-9\-]*\b')

MODULE_REGEX_BASE = '([A-Za-z][A-Za-z0-9\-]*)\s*({\s*([A-Za-z0-9\-]+(\s*\([0-9]+\))?\s*)+})?(\s*--.*?\n)*\s*DEFINITIONS\s*?([A-Z\s]+)?\s*?::=\s*?([A-Z\s]+)?\s*?BEGIN'
MODULE_REGEX = re.compile(fr'^\s*{MODULE_REGEX_BASE}', re.S)

MACRO_STATE_DEFINITION = 0
MACRO_STATE_BEGIN = 1
MACRO_STATE_END = 2

def extract_modules(source: str, full_text: str) -> List[str]:
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
                nonlocal source
                nonlocal cur_line_index
                cur_line_index = full_text.rfind('\n', 0, cur_line_index)
                if cur_line_index == -1:
                    print(f'failed to match MODULE_REGEX for doc {source}')
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
                        print(f'warning: module without END from doc {source}')
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
