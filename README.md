# ASN.1⬢Chef

[![CI](https://github.com/asn1chef/asn1chef/actions/workflows/asn1chef.yml/badge.svg)](https://github.com/asn1chef/asn1chef/actions/workflows/asn1chef.yml)

ASN.1⬢Chef is a web-based suite of tools for working with [Abstract Syntax Notation One (ASN.1)](https://en.wikipedia.org/wiki/ASN.1).

The [ASN.1⬢Chef Editor](https://asn1chef.github.io/asn1chef) provides a Visual Studio Code-based editor for working with ASN.1 modules.

Functionality includes:

- [x] Compiling ASN.1 modules
  - [x] All 35 built-in types
  - [x] Constraints (currently only enforced when encoding, not when decoding)
    - [x] Single Value and Value Range
    - [x] Size
    - [x] Inner Type (parsed but not enforced at this time)
    - [x] Contents
    - [ ] Permitted Alphabet
    - [ ] Pattern
    - [ ] Properties
    - [ ] Type
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
- [x] Encoding defined values into various tranfer syntaxes
  - [ ] BER (Basic Encoding Rules)
    - [ ] CER (Canonical Encoding Rules)
    - [x] DER (Distinguished Encoding Rules)
  - [ ] JER (JSON Encoding Rules)
  - [ ] PER (Packed Encoding Rules)
    - [ ] UPER (Unaligned Packed Encoding Rules)
    - [ ] CPER (Canonical Packed Encoding Rules)
    - [ ] CUPER (Canonical Unaligned Packed Encoding Rules)
  - [ ] OER (Octet Encoding Rules)
  - [ ] XER (XML Encoding Rules)
- [x] Decoding arbitrary encoded bytes into types defined in ASN.1 modules
  - [ ] Editing parsed data directly in the UI
  - [ ] Converting data between ASN.1 encodings
- [x] Package registry with downloadable ASN.1 modules from various standards, including from:
  - [x] IETF (Internet Engineering Task Force)
  - [x] ITU-T (International Telecommunication Union Telecommunication Standardization Sector)
  - [ ] 3GPP (3rd Generation Partnership Project)
  - [ ] User-defined custom sources
  - [x] Support for dependencies between packages
- [ ] Code Generation
  - [ ] Producing type stubs representing ASN.1 type definitions
  - [ ] Producing functions to encode/decode stubs with various ASN.1 encodings
