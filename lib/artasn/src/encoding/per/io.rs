use std::io::Write;

use num::BigInt;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Alignment {
    // Don't align the value, even if using aligned PER.
    None,
    // The bits are aligned to the start of a byte boundary, so padding bits are appended to the right.
    Start,
    // The bits are aligned to the end of a byte bondary, so padding bits are appended to the left.
    End,
}

pub struct BitWriter<W: Write> {
    aligned: bool,
    writer: W,
    partial_byte: u8,
    bit_cursor: u8,
}

impl<W: Write> BitWriter<W> {
    pub fn new(aligned: bool, writer: W) -> BitWriter<W> {
        BitWriter {
            aligned,
            writer,
            partial_byte: 0,
            bit_cursor: 0,
        }
    }

    pub fn write_int(&mut self, int: u64, bits: u32, alignment: Alignment) {
        if self.aligned && alignment == Alignment::End && bits % 8 != 0 {
            let align_bits = 8 - (bits % 8);
            for _ in 0..align_bits {
                self.write_bit(false);
            }
        }

        for bit_pos in (0..bits).rev() {
            let bit = (int >> bit_pos) & 1;
            self.write_bit(bit == 1);
        }

        if self.aligned && alignment == Alignment::Start {
            self.align();
        }
    }

    pub fn write_bigint(&mut self, int: &BigInt, bits: u64, alignment: Alignment) {
        if self.aligned && alignment == Alignment::End && bits % 8 != 0 {
            let align_bits = 8 - (bits % 8);
            for _ in 0..align_bits {
                self.write_bit(false);
            }
        }

        for bit_pos in (0..bits).rev() {
            self.write_bit(int.bit(bit_pos));
        }

        if self.aligned && alignment == Alignment::Start && bits % 8 != 0 {
            let align_bits = 8 - (bits % 8);
            for _ in 0..align_bits {
                self.write_bit(false);
            }
        }
    }

    pub fn write_bytes(&mut self, bytes: &[u8]) {
        if self.aligned {
            if self.bit_cursor != 0 {
                panic!("write_bytes: aligned but bit_cursor == {}", self.bit_cursor);
            }
            self.writer.write_all(bytes).expect("write bytes");
        } else {
            for byte in bytes {
                self.write_byte(*byte);
            }
        }
    }

    pub fn write_byte(&mut self, byte: u8) {
        if self.aligned {
            if self.bit_cursor != 0 {
                panic!("write_byte: aligned but bit_cursor == {}", self.bit_cursor);
            }
            self.writer.write_all(&[byte]).expect("write bytes");
        } else {
            for bit_pos in (0..8).rev() {
                let bit = (byte >> bit_pos) & 1;
                self.write_bit(bit == 1);
            }
        }
    }

    pub fn align(&mut self) {
        if self.aligned {
            self.force_align();
        }
    }

    pub fn force_align(&mut self) {
        while self.bit_cursor > 0 {
            self.write_bit(false);
        }
    }

    #[inline(always)]
    pub fn write_bit(&mut self, bit: bool) {
        if bit {
            self.partial_byte |= 1 << (7 - self.bit_cursor);
        }

        if self.bit_cursor == 7 {
            self.writer
                .write_all(&[self.partial_byte])
                .expect("write byte");
            self.bit_cursor = 0;
            self.partial_byte = 0;
        } else {
            self.bit_cursor += 1;
        }
    }
}

#[cfg(test)]
mod test {
    use std::io::Cursor;

    use crate::encoding::per::Alignment;

    use super::BitWriter;

    fn exec_test<F: Fn(BitWriter<Cursor<&mut Vec<u8>>>)>(f: F) -> (Vec<u8>, Vec<u8>) {
        let mut aligned_buf = Vec::new();
        let writer = BitWriter::new(true, Cursor::new(&mut aligned_buf));
        f(writer);

        let mut unaligned_buf = Vec::new();
        let writer = BitWriter::new(false, Cursor::new(&mut unaligned_buf));
        f(writer);

        (aligned_buf, unaligned_buf)
    }

    #[test]
    pub fn test_bit_writer_write_full_byte() {
        let buf = exec_test(|mut writer| {
            for _ in 0..8 {
                writer.write_bit(true);
            }
        });
        assert_eq!(buf, (vec![0xff], vec![0xff]));

        let buf = exec_test(|mut writer| {
            for _ in 0..8 {
                writer.write_bit(false);
            }
        });
        assert_eq!(buf, (vec![0x00], vec![0x00]));

        let buf = exec_test(|mut writer| {
            for i in 0..8 {
                writer.write_bit(i % 2 == 0);
            }
        });
        assert_eq!(buf, (vec![0xaa], vec![0xaa]));

        let buf = exec_test(|mut writer| {
            for i in 0..8 {
                writer.write_bit(i % 2 == 1);
            }
        });
        assert_eq!(buf, (vec![0x55], vec![0x55]));
    }

    #[test]
    pub fn test_bit_writer_write_partial_byte() {
        let buf = exec_test(|mut writer| {
            for _ in 0..7 {
                writer.write_bit(true);
            }
        });
        assert_eq!(buf, (vec![], vec![]));

        let buf = exec_test(|mut writer| {
            for _ in 0..15 {
                writer.write_bit(true);
            }
        });
        assert_eq!(buf, (vec![0xff], vec![0xff]));
    }

    #[test]
    pub fn test_bit_writer_write_int() {
        let buf = exec_test(|mut writer| {
            writer.write_int(6, 3, Alignment::Start);
            writer.force_align();
        });
        assert_eq!(buf, (vec![0xC0], vec![0xC0]));

        let buf = exec_test(|mut writer| {
            writer.write_int(6, 3, Alignment::End);
            writer.force_align();
        });
        assert_eq!(buf, (vec![0x06], vec![0xC0]));

        let buf = exec_test(|mut writer| {
            writer.write_int(2000, 11, Alignment::Start);
            writer.force_align();
        });
        assert_eq!(buf, (vec![0xfa, 0x00], vec![0xfa, 0x00]));

        let buf = exec_test(|mut writer| {
            writer.write_int(2000, 11, Alignment::End);
            writer.force_align();
        });
        assert_eq!(buf, (vec![0x07, 0xd0], vec![0xfa, 0x00]));
    }
}
