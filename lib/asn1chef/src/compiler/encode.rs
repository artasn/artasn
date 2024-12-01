pub fn write_vlq(mut n: u64, buf: &mut Vec<u8>) -> usize {
    if n < 0x80 {
        buf.push(n as u8);
        return 1;
    }

    const BUF_SIZE: usize = std::mem::size_of::<u64>() * 8 / 7 + 1;
    let mut vlq_buf = [0u8; BUF_SIZE];
    let mut index = 0;

    while n >= 0x80 {
        vlq_buf[index] = (n & 0b0111_1111) as u8;
        index += 1;
        n >>= 7;
    }

    vlq_buf[index] = 0b1000_0000 | n as u8;
    index += 1;
    buf.extend_from_slice(&vlq_buf[..index]);
    index
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
