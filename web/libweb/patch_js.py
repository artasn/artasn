import sys

def main():
    js_file = sys.argv[1]
    with open(js_file, 'r') as f:
        js = f.read()

    js = js.replace("module_or_path = new URL('libasn1chef_bg.wasm', import.meta.url);", "throw new Error('module_or_path unspecified');")

    with open(js_file, 'w') as f:
        f.write(js)

if __name__ == '__main__':
    main()
