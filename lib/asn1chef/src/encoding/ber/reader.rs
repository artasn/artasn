use crate::encoding::*;

use super::MAX_VLQ_LEN;

pub(crate) fn read_vlq(buf: &[u8]) -> io::Result<(u64, usize)> {
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

pub(crate) fn read_tlv_len(buf: &[u8]) -> io::Result<(u64, usize)> {
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

#[cfg(test)]
mod test {
    use super::read_vlq;

    fn vlq_to_val(vlq: &[u8]) -> u64 {
        let (val, _) = read_vlq(vlq).unwrap();
        val
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
}
