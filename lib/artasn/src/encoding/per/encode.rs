use std::io::Write;

use num::BigInt;

use crate::{
    compiler::{parser::Result, Context},
    types::*,
    values::*,
};

use super::{Alignment, BitWriter, PerEncoder};

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

fn write_per_vlq<W: Write>(writer: &mut BitWriter<W>, n: u64, max: Option<u64>) {
    writer.align();

    let max = max.unwrap_or(n);
    if max < 0x80 {
        writer.write_byte(n as u8);
    } else if max < 0x4000 {
        let msb = ((n >> 8) & 0xff) as u8;
        let lsb = (n & 0xff) as u8;
        writer.write_byte(0x80 | msb);
        writer.write_byte(lsb);
    } else {
        todo!("encode fragmented values");
    }
}

enum LengthDeterminantKind {
    /// No length determinant is encoded, because it is constrained to one value.
    ConstLength,
    /// The length determinant is encoded as a fixed number of bits, because it is constrained to a range of values.
    FixedSizeLength,
    /// The length determinant is encoded as a variable number of bits, because it is unbounded.
    VariableSizeLength,
}

fn write_length_determinant<W: Write>(
    writer: &mut BitWriter<W>,
    length: u64,
    length_range: (Option<u64>, Option<u64>),
) -> LengthDeterminantKind {
    let (length_min, length_max) = length_range;
    match (length_min, length_max) {
        (Some(min), Some(max)) if min == max => LengthDeterminantKind::ConstLength,
        (_, None) => {
            write_per_vlq(writer, length, None);
            LengthDeterminantKind::VariableSizeLength
        }
        (low, Some(high @ ..65536)) => {
            write_int(writer, length, low.unwrap_or(0), high, Alignment::None);
            LengthDeterminantKind::FixedSizeLength
        }
        (_, Some(length_max @ 65536..)) => {
            write_per_vlq(writer, length, Some(length_max));
            LengthDeterminantKind::VariableSizeLength
        }
    }
}

fn write_size_determinant<W: Write>(
    writer: &mut BitWriter<W>,
    context: &Context,
    size: u64,
    resolved_type: &ResolvedType,
) -> Result<LengthDeterminantKind> {
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
                panic!("per_encode_value on {} with illegal size", resolved_type.ty)
            }
        }
    }

    Ok(write_length_determinant(writer, size, size_bounds))
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

    write_per_vlq(&mut encoder.writer, tmp_buf.len() as u64, None);
    encoder.writer.write_bytes(tmp_buf);
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
        BuiltinValue::BitString(bs) => {
            let mut total_bits = bs.data.len() as u64 * 8 - bs.unused_bits as u64;

            match &typed_value.ty.ty {
                BuiltinType::BitString(bs) => {
                    if bs.named_bits.is_some() {
                        if let Some(constraint) = &typed_value.ty.constraint {
                            if let Some(bounds) = constraint.size_bounds(context)? {
                                match bounds.lower_bound {
                                    Bound::Integer(lower_bound) => {
                                        let lower_bound = lower_bound
                                            .try_into()
                                            .expect("lower bound is out of bounds");
                                        if total_bits < lower_bound {
                                            // BIT STRING with named bits are permitted to have values whose size are less than the lower bound
                                            // in PER, the lower bound just indicates how much padding to add to the value
                                            total_bits = lower_bound;
                                        }
                                    }
                                    Bound::Unbounded => (),
                                }
                            }
                        }
                    }
                }
                _ => unreachable!(),
            };

            let determinant_kind =
                write_size_determinant(&mut encoder.writer, context, total_bits, &typed_value.ty)?;
            match determinant_kind {
                LengthDeterminantKind::ConstLength => (),
                _ => {
                    if total_bits > 0 {
                        encoder.writer.align();
                    }
                }
            }
            for bit_index in 0..total_bits {
                let byte_index = (bit_index / 8) as usize;
                if byte_index >= bs.data.len() {
                    encoder.writer.write_bit(false);
                } else {
                    let byte = bs.data[byte_index];
                    let bit_pos = 7 - (bit_index % 8) as usize;
                    let bit = (byte >> bit_pos) & 1;
                    encoder.writer.write_bit(bit == 1);
                }
            }
        }
        BuiltinValue::OctetString(bytes) => {
            let determinant_kind = write_size_determinant(
                &mut encoder.writer,
                context,
                bytes.len() as u64,
                &typed_value.ty,
            )?;
            match determinant_kind {
                LengthDeterminantKind::ConstLength => (),
                _ => {
                    if !bytes.is_empty() {
                        encoder.writer.align();
                    }
                }
            }
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
        test_per_encode_bit_string,
        "../../../test-data/encode/per/PerBitStringTest"
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
