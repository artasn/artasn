default: generate-package-registry

.PHONY: build-libdeps generate-package-registry

build-libdeps:
	cd libdeps && \
	cargo build --release && \
	cd ..

generate-package-registry: build-libdeps
	poetry run python generate-package-registry.py
