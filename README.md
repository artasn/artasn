# ASN1Chef

ASN1Chef is a web-based suite of tools for working with [Abstract Syntax Notation One (ASN.1)](https://en.wikipedia.org/wiki/ASN.1).

The [ASN1Chef Editor](https://lucasbaizer2.github.com/asn1chef) provides a Visual Studio Code-based editor for working with ASN.1 modules.

Functionality includes:

- [x] Compiling ASN.1 modules
  - [x] All UNIVERSAL types (most are implemented currently, but not all)
  - [ ] Size and value constraints (can be parsed, but not enforced)
  - [ ] Parmeterized types
  - [ ] CLASS types
  - [ ] MACRO definitions
- [x] Visual Studio Code editor window
  - [x] ASN.1 extension providing features including:
    - [x] Syntax highlighting
    - [x] Error and warning messages
    - [ ] Completion options
    - [ ] Jump to declaration and references
    - [ ] Hover information
  - [ ] Importing ASN.1 files and archives containing ASN.1 files from the user's file system into the VSCode editor
  - [ ] Exporting ASN.1 files from the VSCode editor to a downloadable compressed archive.
- [x] Encoding defined values into various encodings
  - [ ] BER (Basic Encoding Rules)
  - [ ] CER (Canonical Encoding Rules)
  - [x] DER (Distinguished Encoding Rules)
  - [ ] JER (JSON Encoding Rules)
  - [ ] XER (XML Encoding Rules)
- [ ] Decoding arbitrary encoded bytes into types defined in ASN.1 modules.
- [ ] Package registry with downloadable ASN.1 modules from various standards, including from:
  - [ ] IETF (Internet Engineering Task Force)
  - [ ] ITU-T (International Telecommunication Union Telecommunication Standardization Sector)
  - [ ] 3GPP (3rd Generation Partnership Project)
