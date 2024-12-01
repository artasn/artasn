# standard-modules

This directory contains tools for retrieving ASN.1 modules for various standards from the Internet.
Sources include IETF, ITU-T, and in the future 3GPP.

## Download Modules

To download modules, use `poetry` to run the Python file you want:

To scrape for IETF modules, use: 

```
poetry run python download-ietf-modules.py
```

To download ITU-T modules:

1. Download all [recommendations](https://www.itu.int/ITU-T/recommendations/search.aspx?type=30&status=F&main=1&pg_size=100).
Click the `Export result to MS Excel file` button, which looks like the icon for Excel.
2. Download all [modules](https://www.itu.int/ITU-T/recommendations/fl.aspx?lang=1&pg_size=100). Click the same button to get the Excel file for the modules.
3. `poetry run python download-itu-t-modules.py <Recommendations.xls> <Modules.xls>`

## Generating Package Registry

Once you have downloaded all modules, the next step is to generate the package registry.

Run `poetry run python generate-package-registry.py`.
This will generate a directory at `data/package-registry`.
The contents of this directory should be statically served from a web server, for use with ASN1Chef.
