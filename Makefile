default: test build

CARGO=cargo

ifndef $(MODE)
	MODE=--release
endif

PROG=asn1chef
WASM_TARGET=wasm32-unknown-unknown 

clean:
	rm -rf target

build: build-cli build-editor

.PHONY: .install-cargo .install-wasm .install-wasm-pack build-cli build-libweb build-webfs build-asn1-extension build-vscode-web build-editor dev-editor test

.install-cargo:
	$(shell which cargo > /dev/null || (echo "Rust and Cargo are required to build $(PROG). Visit https://rustup.rs/ to do so") && exit 1)

build-cli: .install-cargo
	cd lib && \
	$(CARGO) build --package cli $(MODE) && \
	cd ..

.install-wasm: .install-cargo
	$(shell rustup target list --installed | grep $(WASM_TARGET) > /dev/null || (rustup target add $(WASM_TARGET)))

.install-wasm-pack: .install-wasm
	$(shell which wasm-pack > /dev/null || cargo install wasm-pack)

build-libweb: .install-cargo .install-wasm .install-wasm-pack
	cd web/libweb && \
	wasm-pack build $(MODE) --target web --no-pack && \
	cd .. && \
	rm -rf editor/src/wasm && \
	cp -r libweb/pkg editor/src/wasm && \
	python3 libweb/patch_js.py editor/src/wasm/libasn1chef.js && \
	mkdir -p editor/public/static && \
	mv editor/src/wasm/libasn1chef_bg.wasm editor/public/static && \
	cd ..

build-webfs:
	cd web/webfs && \
	yarn && \
	yarn build && \
	cd ../webfs-extension && \
	yarn && \
	yarn build && \
	rm -rf ../editor/public/static/extensions/webfs && \
	mkdir -p ../editor/public/static/extensions/webfs && \
	cp -r package.json package.nls.json dist ../editor/public/static/extensions/webfs/

build-asn1-extension:
	cd web/asn1-extension && \
	yarn && \
	yarn build && \
	rm -rf ../editor/public/static/extensions/asn1 && \
	mkdir -p ../editor/public/static/extensions/asn1 && \
	cp -r package.json package.nls.json config icons dist ../editor/public/static/extensions/asn1/

build-vscode-web:
	cd web/vscode-web && \
	yarn && \
	yarn build && \
	cd ../..

build-editor: build-libweb build-webfs build-asn1-extension
	export NODE_OPTIONS=--openssl-legacy-provider && \
	cd web/editor && \
	yarn && \
	rm -rf public/static/vs && \
	cp -r node_modules/vscode-web/dist public/static/vs && \
	rm -rf public/static/extensions/theme-defaults && \
	mv public/static/vs/extensions/theme-defaults public/static/extensions/ && \
	rm -rf public/static/extensions/theme-seti && \
	mv public/static/vs/extensions/theme-seti public/static/extensions/ && \
	rm -rf public/static/vs/extensions && \
	yarn build && \
	cd ../..

dev-editor: build-editor
	export NODE_OPTIONS=--openssl-legacy-provider && \
	cd web/editor && \
	yarn dev

test:
	cd lib && \
	$(CARGO) test -- --test-threads=1 --nocapture && \
	cd ..
