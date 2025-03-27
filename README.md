# ArtASN

[![CI](https://github.com/artasn/artasn/actions/workflows/artasn.yml/badge.svg)](https://github.com/artasn/artasn/actions/workflows/artasn.yml)

ArtASN is an experimental suite of tools for working with [Abstract Syntax Notation One (ASN.1)](https://en.wikipedia.org/wiki/ASN.1).
Module compilation, encoding values defined in ASN.1 syntax, decoding values into their respective ASN.1 types, and dependencies (such as on standard modules declared in standards from IETF or ITU-T) are supported via a CLI and the [ArtASN Playground](#artasn-playground).

### ArtASN Playground

The [ArtASN Playground](https://artasn.github.io/artasn) (WIP) provides a Visual Studio Code editor in the web browser for learning, experimentation, and reverse engineering ASN.1 modules and data.

### Features

Features currently include:

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
  - [x] Parameterized types
  - [x] X.681: Information object classes, objects, and object sets
  - [ ] MACRO definitions
- [x] ArtASN Playground
  - [x] Visual Studio Code editor window
    - [x] ASN.1 extension providing features including:
      - [x] Syntax highlighting
      - [x] Error and warning messages
      - [ ] Completion options
      - [ ] Jump to declaration and references
      - [ ] Hover information
    - [ ] Importing ASN.1 files and archives containing ASN.1 files from the user's file system into the VSCode editor
    - [ ] Exporting ASN.1 files from the VSCode editor to a downloadable compressed archive.
- [ ] Local ArtASN Compiler Server with VSCode for Desktop Extension with Playground UI
- [ ] Cargo-like compiler frontend to declare projects with dependencies
- [x] Encoding defined values into various tranfer syntaxes
  - [ ] [BER](https://en.wikipedia.org/wiki/X.690#BER,_CER_and_DER_compared)
  - [ ] [CER](https://en.wikipedia.org/wiki/X.690#BER,_CER_and_DER_compared)
  - [x] [DER](https://en.wikipedia.org/wiki/X.690#BER,_CER_and_DER_compared)
  - [ ] [JER](https://www.oss.com/asn1/resources/asn1-papers/Overview_of_JER.pdf)
  - [ ] [PER](https://www.oss.com/asn1/resources/asn1-made-simple/asn1-quick-reference/packed-encoding-rules.html)
  - [ ] [UPER](https://www.oss.com/asn1/resources/asn1-made-simple/asn1-quick-reference/packed-encoding-rules.html)
  - [ ] [CPER](https://www.oss.com/asn1/resources/asn1-made-simple/asn1-quick-reference/packed-encoding-rules.html)
  - [ ] [CUPER](https://www.oss.com/asn1/resources/asn1-made-simple/asn1-quick-reference/packed-encoding-rules.html)
  - [ ] [OER](https://www.oss.com/asn1/resources/books-whitepapers-pubs/Overview_of_OER.pdf)
  - [ ] [COER](https://www.oss.com/asn1/resources/books-whitepapers-pubs/Overview_of_OER.pdf)
  - [ ] [XER](https://www.itu.int/en/ITU-T/asn1/Pages/xer.aspx)
  - [ ] [CXER](https://www.itu.int/en/ITU-T/asn1/Pages/xer.aspx)
  - [ ] [E-XER](https://www.itu.int/en/ITU-T/asn1/Pages/xer.aspx)
- [ ] [Encoding Control Notation](https://www.itu.int/en/ITU-T/asn1/Pages/ecn.aspx) (ECN)
- [x] Decoding arbitrary encoded bytes into types defined in ASN.1 modules
  - [ ] Editing parsed data directly in the ArtASN Playground UI
  - [ ] Converting data between ASN.1 transfer syntaxes
- [x] Package registry with downloadable ASN.1 modules from various standards, including from:
  - [x] IETF (Internet Engineering Task Force)
  - [x] ITU-T (International Telecommunication Union Telecommunication Standardization Sector)
  - [ ] 3GPP (3rd Generation Partnership Project)
  - [ ] User-defined custom sources
  - [x] Support for dependencies between packages
- [ ] Code Generation
  - [ ] Producing type stubs representing ASN.1 type definitions
  - [ ] Producing functions to encode/decode stubs with various ASN.1 transfer syntaxes
