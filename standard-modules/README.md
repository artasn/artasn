# standard-modules

This directory contains tools for retrieving ASN.1 modules for various standards from the Internet.
Sources include IETF, ITU-T, and in the future 3GPP.

## Download Modules

To download modules, use [Poetry](https://python-poetry.org/docs/#installation) to run the Python file you want.

### IETF

To scrape for IETF modules, use: 

```
poetry run python download-ietf-modules.py
```

### ITU-T

To download ITU-T modules:

1. Download all [recommendations](https://www.itu.int/ITU-T/recommendations/search.aspx?type=30&status=F&main=1&pg_size=100).
Click the `Export result to MS Excel file` button, which looks like the icon for Excel.
2. Download all [modules](https://www.itu.int/ITU-T/recommendations/fl.aspx?lang=1&pg_size=100). Click the same button to get the Excel file for the modules.
3. `poetry run python download-itu-t-modules.py <Recommendations.xls> <Modules.xls>`

Note that despite the `.xls` extension, the exported files are actually HTML tables, not real spreadsheets.
This is expected: pass the downloaded files to the script as-is, without opening or converting them.

### 3GPP

To download 3GPP modules, first install the necessary system dependencies:

```
sudo apt install -y antiword pandoc
```

### Patches

Some published modules contain errors that prevent ArtASN from parsing them.
After downloading, the IETF and ITU-T scripts apply the fixes in [`patch/`](patch/),
which mirrors the layout of each organization's downloaded modules directory:
`patch/<org>/[<subdir>/]<ModuleName>.patch` is applied to `data/<org>/modules/[<subdir>/]<ModuleName>.asn`.
Each patch is a unified diff with paths relative to the module's own directory, e.g. created with:

```
diff -u --label a/<ModuleName>.asn --label b/<ModuleName>.asn original.asn fixed.asn > patch/<org>/<ModuleName>.patch
```

Applying patches is idempotent, and a patch that no longer applies cleanly stops the script with an error.

## Generating Package Registry

Once you have downloaded all modules, the next step is to generate the package registry.

Run `poetry run python generate-package-registry.py`.
This will generate a directory at `data/package-registry`.
The contents of this directory should be statically served from a web server, for use with ArtASN.
