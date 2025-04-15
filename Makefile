default: test build

CARGO=cargo

ifndef $(MODE)
	MODE=--release
endif

PROG=artasn
WASM_TARGET=wasm32-unknown-unknown 

clean:
	rm -rf target

build: build-cli build-playground

.PHONY: .install-cargo .install-wasm .install-wasm-pack build-cli build-cli-deb build-libweb-playground build-libweb-vscode build-webfs build-asn1-extension-playground build-asn1-extension-desktop build-vscode-web build-playground dev-playground test

.install-cargo:
	$(shell which cargo > /dev/null || (echo "Rust and Cargo are required to build $(PROG). Visit https://rustup.rs/ to do so") && exit 1)

build-cli: .install-cargo
	cd lib && \
	$(CARGO) build --package cli $(MODE) && \
	cd ..

BUILD_CLI_DIR=/tmp/artasn/build-cli-deb
build-cli-deb: build-cli
	rm -rf $(BUILD_CLI_DIR) && \
	mkdir -p $(BUILD_CLI_DIR) && \
	mkdir -p $(BUILD_CLI_DIR)/usr/local/bin && \
	cp -r ./lib/cli/DEBIAN $(BUILD_CLI_DIR) && \
	cp ./lib/target/release/artasn $(BUILD_CLI_DIR)/usr/local/bin/ && \
	dpkg-deb --build $(BUILD_CLI_DIR) && \
	cp -r $(BUILD_CLI_DIR).deb ./artasn.deb && \
	rm -rf $(BUILD_CLI_DIR) $(BUILD_CLI_DIR).deb

.install-wasm: .install-cargo
	$(shell rustup target list --installed | grep $(WASM_TARGET) > /dev/null || (rustup target add $(WASM_TARGET)))

.install-wasm-pack: .install-wasm
	$(shell which wasm-pack > /dev/null || cargo install wasm-pack)

build-libweb-playground: .install-cargo .install-wasm .install-wasm-pack
	cd web/libweb && \
	PARSEGEN_TS_BINDINGS=../../web/asn1-extension-protocol/src/asn1-ast.d.ts \
	  wasm-pack build $(MODE) --target web --no-pack -- --features code && \
	cd .. && \
	rm -rf playground/src/wasm && \
	cp -r libweb/pkg playground/src/wasm && \
	python3 libweb/patch_js.py playground/src/wasm/libartasn.js && \
	mkdir -p playground/public/static && \
	mv playground/src/wasm/libartasn_bg.wasm playground/public/static && \
	cd ..

build-libweb-vscode: .install-cargo .install-wasm .install-wasm-pack
	cd web/libweb && \
	PARSEGEN_TS_BINDINGS=../../web/asn1-extension-protocol/src/asn1-ast.d.ts \
	  wasm-pack build $(MODE) --target nodejs --no-pack -- --features code && \
	cd .. && \
	cp libweb/pkg/libartasn.js libweb/pkg/libartasn.d.ts asn1-extension/src/desktop/wasm && \
	mkdir -p asn1-extension/dist/desktop && \
	cp libweb/pkg/libartasn_bg.* asn1-extension/dist/desktop

build-webfs:
	cd web/webfs && \
	yarn && \
	yarn build && \
	cd ../webfs-extension && \
	yarn && \
	yarn build && \
	rm -rf ../playground/public/static/extensions/webfs && \
	mkdir -p ../playground/public/static/extensions/webfs && \
	cp -r package.json package.nls.json dist ../playground/public/static/extensions/webfs/

build-asn1-extension-playground: build-libweb-playground
	cd web/asn1-extension && \
	yarn && \
	yarn build-web && \
	rm -rf ../playground/public/static/extensions/asn1 && \
	mkdir -p ../playground/public/static/extensions/asn1 && \
	cp -r package.json package.nls.json config icons dist ../playground/public/static/extensions/asn1/

build-asn1-extension-desktop: build-libweb-vscode
	cd web/asn1-extension && \
	yarn && \
	yarn build-desktop

build-vscode-web:
	cd web/vscode-web && \
	yarn && \
	yarn build && \
	cd ../..

build-playground: build-libweb-playground build-webfs build-asn1-extension-playground
	export NODE_OPTIONS=--openssl-legacy-provider && \
	cd web/playground && \
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

dev-playground: build-playground
	export NODE_OPTIONS=--openssl-legacy-provider && \
	cd web/playground && \
	yarn dev

test:
	cd lib && \
	$(CARGO) test -- --nocapture && \
	cd ..
