//! All of the charset implementations in this file were based on the ITU-T.X680 (02/2021) standard

use std::marker::PhantomData;

#[derive(Debug, Clone)]
pub struct NumericStringCharset {}

impl CharacterStringCharset for NumericStringCharset {
    fn accepts(c: char) -> bool {
        c.is_ascii_digit() || c == ' '
    }
}

#[derive(Debug, Clone)]
pub struct PrintableStringCharset {}

const PRINTABLE_CHARS: &str = " '()+,-./:=?";

impl CharacterStringCharset for PrintableStringCharset {
    fn accepts(c: char) -> bool {
        c.is_ascii_alphanumeric() || PRINTABLE_CHARS.contains(c)
    }
}

#[derive(Debug, Clone)]
pub struct CharacterString<Charset: CharacterStringCharset> {
    _phantom: PhantomData<Charset>,
    text: String,
}

impl<'a, Charset> Default for CharacterString<Charset>
where
    Charset: CharacterStringCharset,
 {
    fn default() -> Self {
        Self::new()
    }
}

impl<'a, Charset> CharacterString<Charset>
where
    Charset: CharacterStringCharset,
{
    pub fn new() -> CharacterString<Charset> {
        CharacterString::<Charset> {
            _phantom: PhantomData,
            text: String::new(),
        }
    }

    pub fn as_str(&'a self) -> &'a str {
        &self.text
    }

    pub fn set_text(&mut self, text: &str) -> bool {
        if text.chars().map(|c| Charset::accepts(c)).any(|a| !a) {
            return false;
        }
        self.text = text.to_string();
        true
    }
}

pub trait CharacterStringCharset {
    fn accepts(c: char) -> bool;
}
