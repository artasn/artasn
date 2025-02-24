import itertools
import os
import shutil
import threading

from typing import List, TypeVar, Callable

def get_data_dir(org: str) -> str:
    if not os.path.exists('data'):
        os.mkdir('data')
    return os.path.join('data', org)


def make_data_dir(org: str) -> str:
    data_dir = get_data_dir(org)
    if os.path.exists(data_dir):
        while True:
            ok = input(f'{org} data directory already exists, do you want to to delete it? (y/n): ').lower()
            if ok == 'y':
                shutil.rmtree(data_dir)
                os.mkdir(data_dir)
                break
            elif ok == 'n':
                break
    else:
        os.mkdir(data_dir)

    return data_dir

T = TypeVar('T')
def chunk_list(lst: List[T], size: int) -> List[List[T]]:
    it = iter(lst)
    return [list(itertools.islice(it, size)) for _ in range((len(lst) + size - 1) // size)]

def execute_chunked_task(items: List[T], task: Callable[[List[T]], None], thread_count=os.cpu_count()):
    print(f'starting {thread_count} chunked task threads')
    threads: List[threading.Thread] = []
    chunked_items = chunk_list(items, len(items) // thread_count)
    for chunk in chunked_items:
        thread = threading.Thread(target=task, args=(chunk,))
        thread.start()
        threads.append(thread)

    for thread in threads:
        thread.join()
