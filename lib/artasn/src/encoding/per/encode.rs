use std::io::Write;

use num::BigInt;

use crate::{
    compiler::{parser::Result, Context},
    encoding::per::MAX_VLQ_LEN,
    types::{Bound, ConstraintCheckMode, IntegerInclusion, ResolvedType, SubtypeElement},
    values::{BuiltinValue, ResolvedValue, ValueResolve},
};

use super::{Alignment, BitWriter, PerEncoder};

fn write_vlq<W: Write>(mut n: u64, writer: &mut BitWriter<W>) -> usize {
    const CARRY_BIT: u8 = 0b1000_0000;
    const MASK: u8 = 0b0111_1111;

    if n < 0x80 {
        writer.write_byte(n as u8);
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
    for i in (0..index).rev() {
        writer.write_byte(vlq_buf[i]);
    }
    index
}

fn write_int<W: Write>(
    writer: &mut BitWriter<W>,
    int: u64,
    range_low: u64,
    range_high: u64,
    alignment: Alignment,
) {
    let max_encoded_value = range_high - range_low;
    let bit_count = u64::BITS - max_encoded_value.leading_zeros();

    let int = int - range_low;
    writer.write_int(int, bit_count, alignment);
}

fn write_length_determinant<W: Write>(
    writer: &mut BitWriter<W>,
    length: u64,
    length_range: (Option<u64>, Option<u64>),
) {
    let (length_min, length_max) = length_range;
    match (length_min, length_max) {
        (Some(min), Some(max)) if min == max => (),
        (_, None) => {
            write_vlq(length, writer);
        }
        (low, Some(high @ ..65536)) => {
            write_int(writer, length, low.unwrap_or(0), high, Alignment::None);
        }
        (_, Some(length_max @ 65536..)) => {
            if length_max <= 127 {
                writer.write_byte(length as u8);
            } else if length_max <= 16383 {
                let mut length_bits = length as u16;
                length_bits &= 0xbfff; // ensure bit 14 is zero
                length_bits |= 1 << 15;
                writer.write_int(length_bits as u64, 16, Alignment::End);
            } else {
                todo!("encode fragmented values");
            }
        }
    }
}

fn write_size_determinant<W: Write>(
    writer: &mut BitWriter<W>,
    context: &Context,
    size: u64,
    resolved_type: &ResolvedType,
) -> Result<()> {
    let (mut size_bounds, inclusion, is_extensible) = match &resolved_type.constraint {
        Some(constraint)
            if constraint
                .find(|element| matches!(element, SubtypeElement::SingleValue(_)))
                .is_none() =>
        {
            let size_bounds = match constraint.size_bounds(context)? {
                Some(bounds) => {
                    let min = match bounds.lower_bound {
                        Bound::Integer(int) => Some(int.try_into().expect("size out of bounds")),
                        Bound::Unbounded => None,
                    };
                    let max = match bounds.upper_bound {
                        Some(Bound::Integer(int)) => {
                            Some(int.try_into().expect("size out of bounds"))
                        }
                        None | Some(Bound::Unbounded) => None,
                    };
                    (min, max)
                }
                None => (None, None),
            };
            let inclusion = constraint.includes_integer(
                context,
                ConstraintCheckMode::Size,
                &BigInt::from(size),
            )?;
            let is_extensible = constraint.is_extensible(None);
            (size_bounds, inclusion, is_extensible)
        }
        _ => ((None, None), None, false),
    };
    if let Some(inclusion) = inclusion {
        match inclusion {
            IntegerInclusion::Included { is_extension } => {
                if is_extension {
                    // write extension marker bit, 1-byte aligned
                    writer.write_int(1, 1, Alignment::Start);

                    // when the size is an element of extended constraints, the length is encoded as a VLQ,
                    // which, for length determinants, is what happens when there are no size bounds at all
                    // so, to emulate this, pretend like there aren't actually any size bounds
                    size_bounds = (None, None);
                } else if is_extensible {
                    // write non-extension market bit
                    writer.write_bit(false);
                }
            }
            IntegerInclusion::NotIncluded => {
                panic!("per_encode_value on OCTET STRING with illegal size")
            }
        }
    }

    write_length_determinant(writer, size, size_bounds);
    writer.align();

    Ok(())
}

fn write_length_prefixed_integer<W: Write>(
    encoder: &mut PerEncoder<'_, W>,
    value: &BigInt,
    range_low: Option<&BigInt>,
) {
    let tmp_buf = &mut encoder.tmp_buf;
    tmp_buf.clear();
    tmp_buf.extend_from_slice(&match range_low {
        Some(range_low) => (value - range_low).to_bytes_be().1,
        None => value.to_signed_bytes_be(),
    });

    write_vlq(tmp_buf.len() as u64, &mut encoder.writer);
    encoder.writer.write_bytes(&tmp_buf);
}

fn write_constrained_integer<W: Write>(
    encoder: &mut PerEncoder<'_, W>,
    int: &BigInt,
    range_low: &BigInt,
    range_high: &BigInt,
) {
    let max_encoded_value = range_high - range_low;
    let bit_count = max_encoded_value.bits();

    // X.691 clause 13, note 1
    let alignment = match bit_count {
        ..8 => Alignment::None,
        8..16 => Alignment::End,
        16.. => {
            write_length_prefixed_integer(encoder, int, Some(range_low));
            return;
        }
    };

    let int = int - range_low;
    encoder.writer.write_bigint(&int, bit_count, alignment);
}

fn per_encode_integer<W: Write>(
    encoder: &mut PerEncoder<'_, W>,
    resolved_type: &ResolvedType,
    value: &BigInt,
) -> Result<()> {
    let context = encoder.context;
    let writer = &mut encoder.writer;
    let (mut size_bounds, inclusion, is_extensible) = match &resolved_type.constraint {
        Some(constraint) => {
            let value_bounds = match constraint.integer_value_bounds(context)? {
                Some(bounds) => {
                    let min = match bounds.lower_bound {
                        Bound::Integer(int) => Some(int),
                        Bound::Unbounded => None,
                    };
                    let max = match bounds.upper_bound {
                        Some(Bound::Integer(int)) => Some(int),
                        None | Some(Bound::Unbounded) => None,
                    };
                    (min, max)
                }
                None => (None, None),
            };
            let inclusion =
                constraint.includes_integer(context, ConstraintCheckMode::Value, value)?;
            let is_extensible = constraint.is_extensible(None);
            (value_bounds, inclusion, is_extensible)
        }
        _ => ((None, None), None, false),
    };
    if let Some(inclusion) = inclusion {
        match inclusion {
            IntegerInclusion::Included { is_extension } => {
                if is_extension {
                    // write extension marker bit, 1-byte aligned
                    writer.write_int(1, 1, Alignment::Start);

                    // when the size is an element of extended constraints, the length is encoded as a VLQ,
                    // which, for length determinants, is what happens when there are no size bounds at all
                    // so, to emulate this, pretend like there aren't actually any size bounds
                    size_bounds = (None, None);
                } else if is_extensible {
                    // write non-extension market bit
                    writer.write_bit(false);
                }
            }
            IntegerInclusion::NotIncluded => {
                panic!("per_encode_integer on INTEGER with illegal value")
            }
        }
    }
    match size_bounds {
        (Some(min), Some(max)) => {
            write_constrained_integer(encoder, value, &min, &max);
        }
        (min, _) => write_length_prefixed_integer(encoder, value, min.as_ref()),
    }

    Ok(())
}

pub fn per_encode_value<W: Write>(
    encoder: &mut PerEncoder<'_, W>,
    typed_value: &ResolvedValue,
) -> Result<()> {
    let context = encoder.context;

    match &typed_value.value {
        BuiltinValue::Boolean(b) => {
            encoder.writer.write_bit(*b);
        }
        BuiltinValue::Integer(int) => {
            per_encode_integer(encoder, &typed_value.ty, int)?;
        }
        BuiltinValue::OctetString(bytes) => {
            write_size_determinant(
                &mut encoder.writer,
                context,
                bytes.len() as u64,
                &typed_value.ty,
            )?;
            encoder.writer.write_bytes(bytes);
        }
        BuiltinValue::Null => (),
        BuiltinValue::SequenceOf(seq_of) => {
            write_size_determinant(
                &mut encoder.writer,
                context,
                seq_of.len() as u64,
                &typed_value.ty,
            )?;
            for element in seq_of {
                let element = element.resolve(context)?;
                per_encode_value(encoder, &element)?;
            }
        }
        other => todo!("{other:#?}"),
    }

    Ok(())
}

#[cfg(test)]
mod test {
    use crate::compiler::test::json_test;

    json_test!(
        test_per_encode_boolean,
        "../../../test-data/encode/per/PerBooleanTest"
    );
    json_test!(
        test_per_encode_integer,
        "../../../test-data/encode/per/PerIntegerTest"
    );
    json_test!(
        test_per_encode_octet_string,
        "../../../test-data/encode/per/PerOctetStringTest"
    );
    json_test!(
        test_per_encode_null,
        "../../../test-data/encode/per/PerNullTest"
    );
}
