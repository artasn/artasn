#![allow(unused_results)]

use byteorder::{BigEndian, WriteBytesExt};
use int_enum::IntEnum;
use std::io::Write;
use varint_rs::VarintWriter;

use crate::{module::{Exports, Module, TagDefault}, values::Oid};

// Magic header, "ASN1CHEF".
const MODULE_MAGIC_HEADER: u64 = 0x41534e3143484546;
const MODULE_VERSION: u32 = 1;

// The index of a string in the string table entries list.
struct ConstTableEntryPointer(pub u32);

#[derive(IntEnum, Clone, Copy, PartialEq)]
#[repr(u8)]
enum ConstKind {
    String = 1,
    ObjectIdentifier = 2,
}

struct ConstTable {
    pub buf: Vec<u8>,
    pub entries: Vec<ConstTableEntry>,
}

impl ConstTable {
    pub fn add_string(&mut self, str: &str) -> ConstTableEntry {
        self.add_entry(ConstKind::String, str.as_bytes())
    }

    pub fn add_object_identifier(&mut self, oid: &Oid) -> ConstTableEntry {
        todo!();
        // self.add_entry(ConstKind::ObjectIdentifier, &oid.0)
    }

    pub fn add_entry(&mut self, kind: ConstKind, bytes: &[u8]) -> ConstTableEntry {
        for entry in &self.entries {
            if entry.kind == kind {
                let entry_bytes =
                    &self.buf[entry.offset as usize..(entry.offset + entry.len) as usize];
                if bytes == entry_bytes {
                    return entry.clone();
                }
            }
        }

        let entry = ConstTableEntry {
            kind,
            offset: self.buf.len() as u32,
            len: bytes.len() as u32,
            index: self.entries.len(),
        };

        self.buf.write_all(bytes);
        self.entries.push(entry.clone());

        entry
    }
}

#[derive(Clone)]
struct ConstTableEntry {
    kind: ConstKind,
    // The pointer to the first byte of the const relative to the start of the const table
    offset: u32,
    // the length of the const in bytes
    len: u32,
    // The index of the entry in the entry table.
    // Not encoded into the bytecode.
    index: usize,
}

/// Serializes a module into the asn1chef bytecode format.
pub fn write_module<'a>(module: &Module, buf: &mut Vec<u8>) {
    let module = &module.header;

    let mut const_table = ConstTable {
        buf: Vec::with_capacity(4096),
        entries: Vec::with_capacity(4096),
    };
    let module_name_entry = const_table.add_string(&module.id.name);
    let module_id_entry = module
        .id
        .oid
        .as_ref()
        .map(|oid| const_table.add_object_identifier(oid));

    buf.write_u64::<BigEndian>(MODULE_MAGIC_HEADER);
    buf.write_u32_varint(MODULE_VERSION);
    
    buf.write_usize_varint(module_name_entry.index);
    if let Some(module_id_entry) = module_id_entry {
        buf.write_u8(1);
        buf.write_usize_varint(module_id_entry.index);
    } else {
        buf.write_u8(0);
    }

    let tag_default: u8 = match module.tag_default {
        TagDefault::Explicit => 0,
        TagDefault::Implicit => 1,
        TagDefault::Automatic => 2,
    };
    let exports_kind: u8 = match module.exports {
        Exports::All => 0,
        Exports::SymbolList(_) => 1,
    };
    let extensibility_implied: u8 = match module.extensibility_implied {
        true => 1,
        false => 0
    };
    let module_flags = tag_default | (exports_kind << 2) | (extensibility_implied << 3);
    buf.write_u8(module_flags);

    if let Exports::SymbolList(symbol_list) = &module.exports {
        buf.write_usize_varint(symbol_list.len());
        for symbol in symbol_list {
            let entry = const_table.add_string(symbol);
            buf.write_usize_varint(entry.index);
        }
    }
    // TODO: write imports, types, and values

    buf.write_usize_varint(const_table.buf.len());
    buf.write_all(&const_table.buf);

    buf.write_usize_varint(const_table.entries.len());
    for entry in &const_table.entries {
        buf.write_u8(entry.kind as u8);
        buf.write_u32_varint(entry.offset);
        buf.write_u32_varint(entry.len);
    }
}
