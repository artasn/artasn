use std::fmt::Display;
use std::io;
use std::mem;

use crate::types::{Class, TypeForm};

mod parse;
pub use parse::*;

mod values;
pub use values::*;

pub mod strings;

const MAX_VLQ_LEN: usize = 10;

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

pub fn write_tlv_len(len: u64, buf: &mut Vec<u8>) {
    if len < 0x80 {
        buf.push(len as u8);
    } else {
        let (le_bytes, le_bytes_len) = u64_to_le_bytes(len);
        buf.extend_from_slice(&le_bytes[..le_bytes_len]);
        buf.push(0x80 | le_bytes_len as u8);
    }
}

pub fn read_tlv_len(buf: &[u8]) -> io::Result<(u64, usize)> {
    if buf[0] < 0x80 {
        Ok((buf[0] as u64, 1))
    } else {
        let be_bytes_len = buf[0] & 0x7f;
        if buf.len() < 1 + be_bytes_len as usize {
            return Err(io::Error::new(
                io::ErrorKind::UnexpectedEof,
                "TLV length ended early",
            ));
        }
        const BUF_SIZE: usize = mem::size_of::<u64>();
        if be_bytes_len as usize > BUF_SIZE {
            return Err(io::Error::new(
                io::ErrorKind::UnexpectedEof,
                "TLV length is longer than 8 bytes",
            ));
        }

        let be_bytes = &buf[1..1 + be_bytes_len as usize];

        let mut be_buf = [0u8; BUF_SIZE];
        be_buf[BUF_SIZE - be_bytes.len()..].copy_from_slice(be_bytes);

        Ok((u64::from_be_bytes(be_buf), 1 + be_bytes_len as usize))
    }
}

pub fn write_vlq(mut n: u64, buf: &mut Vec<u8>) -> usize {
    const CARRY_BIT: u8 = 0b1000_0000;
    const MASK: u8 = 0b0111_1111;

    if n < 0x80 {
        buf.push(n as u8);
        return 1;
    }

    let mut vlq_buf = [0u8; MAX_VLQ_LEN];
    let mut index = 0;

    while n >= 0x80 {
        vlq_buf[index] = (n & MASK as u64) as u8 | CARRY_BIT;
        index += 1;
        n >>= 7;
    }

    vlq_buf[index] = n as u8 | CARRY_BIT;
    vlq_buf[0] &= MASK;
    index += 1;
    buf.extend_from_slice(&vlq_buf[..index]);
    index
}

pub fn read_vlq(buf: &[u8]) -> io::Result<(u64, usize)> {
    let mut value: u64 = 0;

    for i in 0..MAX_VLQ_LEN {
        if i == buf.len() {
            return Err(io::Error::new(
                io::ErrorKind::UnexpectedEof,
                "VLQ data ended early",
            ));
        }
        let b = buf[i];
        value = value
            .checked_mul(128)
            .and_then(|value| value.checked_add((b & 0x7f) as u64))
            .ok_or_else(|| io::Error::new(io::ErrorKind::InvalidData, "VLQ data overflowed"))?;
        if b < 0x80 {
            return Ok((value, i + 1));
        }
    }

    Err(io::Error::new(
        io::ErrorKind::InvalidData,
        "VLQ data too large",
    ))
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

pub struct DerReader<'a> {
    source: &'a [u8],
    source_start: usize,
    offset: usize,
}

impl<'a> DerReader<'a> {
    pub fn new(source: &'a [u8], source_start: usize) -> DerReader<'a> {
        DerReader {
            source,
            source_start,
            offset: 0,
        }
    }

    pub fn read_all(&mut self) -> io::Result<Vec<Tlv<'a>>> {
        let mut tags = Vec::new();
        while let Some(tag) = self.read_next()? {
            tags.push(tag);
        }
        Ok(tags)
    }

    pub fn read_next(&mut self) -> io::Result<Option<Tlv<'a>>> {
        const TAG_MASK: u8 = 0b11111;

        if self.offset == self.source.len() {
            return Ok(None);
        }

        let tag_start = self.offset;
        let tag_prefix = self.source[self.offset];
        self.offset += 1;

        let class = match (tag_prefix >> 6) & 0b11 {
            0b00 => Class::Universal,
            0b01 => Class::Application,
            0b10 => Class::ContextSpecific,
            0b11 => Class::Private,
            _ => unreachable!(),
        };
        let form = match (tag_prefix >> 5) & 0b1 {
            0b0 => TypeForm::Primitive,
            0b1 => TypeForm::Constructed,
            _ => unreachable!(),
        };
        let num = tag_prefix & TAG_MASK;
        let num = if num & TAG_MASK == TAG_MASK {
            if self.offset >= self.source.len() {
                return Err(io::Error::new(
                    io::ErrorKind::InvalidData,
                    "TLV malformed: EOF before large tag",
                ));
            }
            let (num, len) = read_vlq(&self.source[self.offset..])?;
            self.offset += len;
            num as u16
        } else {
            num as u16
        };

        if self.offset >= self.source.len() {
            return Err(io::Error::new(
                io::ErrorKind::InvalidData,
                "TLV malformed: EOF before length",
            ));
        }

        let tag_end = self.offset;
        let len_start = self.offset;
        let (value_len, len) = read_tlv_len(&self.source[self.offset..])?;
        self.offset += len;
        let len_end = self.offset;

        if self.offset + value_len as usize > self.source.len() {
            return Err(io::Error::new(
                io::ErrorKind::InvalidData,
                format!(
                    "TLV value larger than buffer size (tag at offset {})",
                    tag_start + self.source_start
                ),
            ));
        }

        let value_start = self.offset;
        let value = &self.source[self.offset..self.offset + value_len as usize];
        self.offset += value.len();
        let value_end = self.offset;

        Ok(Some(Tlv {
            tag: TlvElement::new(
                TlvTag { class, form, num },
                TlvPos::new(tag_start + self.source_start, tag_end + self.source_start),
            ),
            len_pos: TlvPos::new(len_start + self.source_start, len_end + self.source_start),
            value: TlvElement::new(
                value,
                TlvPos::new(
                    value_start + self.source_start,
                    value_end + self.source_start,
                ),
            ),
        }))
    }
}

impl<'a> Iterator for DerReader<'a> {
    type Item = io::Result<Tlv<'a>>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.read_next() {
            Ok(option) => option.map(Ok),
            Err(err) => Some(Err(err)),
        }
    }
}

// pub fn encode_der_string(from_literal: StringKind, to_tag: TagType, mut sbits: &str) -> String {
//     let (bit_modulus, pad_size) = match from_literal {
//         StringKind::BString => (8, 1),
//         StringKind::HString => (2, 4),
//         _ => unreachable!(),
//     };
//     let pad_len = match sbits.len() {
//         0 => match from_literal {
//             StringKind::BString => 8,
//             StringKind::HString => {
//                 sbits = "";
//                 0
//             }
//             _ => unreachable!(),
//         },
//         len => {
//             let padding = bit_modulus - (len % bit_modulus);
//             if padding == bit_modulus {
//                 0
//             } else {
//                 padding
//             }
//         }
//     };
//     let mut dbits = String::with_capacity(sbits.len() + pad_len);
//     dbits.write_str(&sbits);
//     for _ in 0..pad_len {
//         dbits.write_char('0');
//     }
//     println!(
//         "sbits: {:X?}, dbits: {:X?}, pad_size={pad_size:} pad_len={pad_len:}",
//         sbits, dbits
//     );

//     let big = match BigUint::from_str_radix(
//         &dbits,
//         match from_literal {
//             StringKind::BString => 2,
//             StringKind::HString => 16,
//             _ => unreachable!(),
//         },
//     ) {
//         Ok(parsed) => parsed,
//         Err(err) => {
//             if format!("{}", err) == "cannot parse integer from empty string" {
//                 BigUint::ZERO
//             } else {
//                 panic!("{:?}", err);
//             }
//         }
//     };

//     let mut buf: Vec<u8> = Vec::new();
//     buf.push(to_tag as u8);
//     let be_bytes = big.to_bytes_be();
//     let len = be_bytes.len();
//     if to_tag == TagType::BitString {
//         if pad_len == 8 {
//             buf.push(1);
//         } else {
//             buf.push(1 + len as u8); // L (pad-len + big-endian bytes)
//             buf.push((pad_len * pad_size) as u8);
//         }
//     } else if to_tag == TagType::OctetString {
//         if len > 0 {
//             buf.push(len as u8);
//         }
//     }
//     buf.extend(be_bytes);

//     return hex::encode_upper(buf);
// }

#[cfg(test)]
mod test {
    use crate::{
        compiler::{test, Context},
        encoding::{DecodeMode, DecodeResult},
    };

    use super::{read_vlq, write_vlq, DecodeError, DecodedValue, DerReader};

    fn val_to_vlq(val: u64) -> Vec<u8> {
        let mut buf = Vec::with_capacity(10);
        write_vlq(val, &mut buf);
        buf.into_iter().rev().collect()
    }

    fn vlq_to_val(vlq: &[u8]) -> u64 {
        let (val, _) = read_vlq(vlq).unwrap();
        val
    }

    #[test]
    fn test_write_vlq() {
        assert_eq!(val_to_vlq(0), vec![0x00]);
        assert_eq!(val_to_vlq(1), vec![0x01]);
        assert_eq!(val_to_vlq(0x7f), vec![0x7f]);
        assert_eq!(val_to_vlq(0x80), vec![0x81, 0x00]);
        assert_eq!(val_to_vlq(0x81), vec![0x81, 0x01]);
        assert_eq!(val_to_vlq(0x3fff), vec![0xff, 0x7f]);
        assert_eq!(val_to_vlq(0x4000), vec![0x81, 0x80, 0x00]);
        assert_eq!(
            val_to_vlq(0xffff_ffff_ffff_ffff),
            vec![0x81, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x7f]
        );
    }

    #[test]
    fn test_read_vlq() {
        assert_eq!(vlq_to_val(&[0x00]), 0);
        assert_eq!(vlq_to_val(&[0x01]), 1);
        assert_eq!(vlq_to_val(&[0x7f]), 0x7f);
        assert_eq!(vlq_to_val(&[0x81, 0x00]), 0x80);
        assert_eq!(vlq_to_val(&[0x81, 0x01]), 0x81);
        assert_eq!(vlq_to_val(&[0xff, 0x7f]), 0x3fff);
        assert_eq!(vlq_to_val(&[0x81, 0x80, 0x00]), 0x4000);
        assert_eq!(
            vlq_to_val(&[0x81, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x7f]),
            0xffff_ffff_ffff_ffff
        );
    }

    #[test]
    fn test_read_invalid_vlq() {
        assert_eq!(
            read_vlq(&[0x80]).unwrap_err().to_string(),
            "VLQ data ended early"
        );
        assert_eq!(
            read_vlq(&[0x80; 10]).unwrap_err().to_string(),
            "VLQ data too large"
        );
        assert_eq!(
            read_vlq(&[0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x7f])
                .unwrap_err()
                .to_string(),
            "VLQ data overflowed"
        );
    }

    #[test]
    fn test_der_decode_contextless() {
        // TODO: once the compiler has the functionality to compile the X.509 modules,
        // change DecodeMode from Contextless to SpecificType with the Certificate type
        let reader = DerReader::new(
            include_bytes!("../../test-data/decode/LetsEncryptX3.der"),
            0,
        );
        let context = Context::new();
        reader
            .into_iter()
            .map(|tlv| {
                DecodedValue::der_decode(
                    &context,
                    tlv.map_err(DecodeError::Io)?,
                    &DecodeMode::Contextless,
                )
            })
            .collect::<DecodeResult<Vec<DecodedValue>>>()
            .unwrap();
    }

    #[test]
    fn test_der_decode_specific_type() {
        let module_file = include_str!("../../test-data/decode/DecodeTest.asn");
        let test_file = include_str!("../../test-data/decode/DecodeTest.test.json");

        test::execute_json_test(module_file, test_file);
    }
}

// #[cfg(test)]
// mod test {
//     use crate::types::TagType;

//     use super::{encode_der_string, parser::StringKind};

//     #[test]
//     fn test_encode_der_strings() {
//         assert_eq!(
//             &encode_der_string(StringKind::BString, TagType::BitString, ""),
//             "030100"
//         );
//         assert_eq!(
//             &encode_der_string(StringKind::HString, TagType::BitString, ""),
//             "03020000"
//         );
//         assert_eq!(
//             &encode_der_string(StringKind::HString, TagType::OctetString, ""),
//             "040100"
//         );
//         assert_eq!(
//             &encode_der_string(StringKind::BString, TagType::BitString, "0"),
//             "03020700"
//         );
//         assert_eq!(
//             &encode_der_string(StringKind::BString, TagType::BitString, "10101"),
//             "030203A8"
//         );
//         assert_eq!(
//             &encode_der_string(StringKind::BString, TagType::BitString, "11010101"),
//             "030200D5"
//         );
//         assert_eq!(
//             &encode_der_string(StringKind::HString, TagType::BitString, "DEADBEEF"),
//             "030500DEADBEEF"
//         );
//         assert_eq!(
//             &encode_der_string(StringKind::HString, TagType::BitString, "DEADBEEF2"),
//             "030604DEADBEEF20"
//         );
//         assert_eq!(
//             &encode_der_string(StringKind::HString, TagType::OctetString, "DEADBEEF"),
//             "0404DEADBEEF"
//         );
//         assert_eq!(
//             &encode_der_string(StringKind::HString, TagType::OctetString, "DEADBEEF2"),
//             "0405DEADBEEF20"
//         );
//     }
// }
