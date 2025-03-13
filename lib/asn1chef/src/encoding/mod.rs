use std::fmt::Display;
use std::io;
use std::mem;

use crate::types::{Class, TypeForm};

mod decode;
pub use decode::*;

mod transfer;
pub use transfer::*;

pub(crate) mod strings;

mod ber;

// Encodes a u64 to the least amount of little-endian bytes required to encode its full value.
fn u64_to_le_bytes(num: u64) -> ([u8; mem::size_of::<u64>()], usize) {
    if num == 0 {
        ([0x00; 8], 1)
    } else {
        let le_bytes = num.to_le_bytes();
        let mut msb_index = le_bytes.len() - 1;
        while le_bytes[msb_index] == 0x00 {
            msb_index -= 1;
        }
        (le_bytes, msb_index + 1)
    }
}





#[derive(Debug, Clone, Copy)]
pub struct TlvPos {
    pub start: usize,
    pub end: usize,
}

impl TlvPos {
    pub fn new(start: usize, end: usize) -> TlvPos {
        TlvPos { start, end }
    }
}

#[derive(Debug, Clone)]
pub struct TlvElement<T> {
    pub element: T,
    pub pos: TlvPos,
}

impl<T> TlvElement<T> {
    pub fn new(element: T, pos: TlvPos) -> TlvElement<T> {
        TlvElement { element, pos }
    }
}

#[derive(Debug, Clone)]
pub struct TlvTag {
    pub class: Class,
    pub form: TypeForm,
    pub num: u16,
}

impl Display for TlvTag {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("[{} {}]", self.class, self.num))
    }
}

#[derive(Debug, Clone)]
pub struct Tlv<'a> {
    pub tag: TlvElement<TlvTag>,
    pub len_pos: TlvPos,
    pub value: TlvElement<&'a [u8]>,
}
