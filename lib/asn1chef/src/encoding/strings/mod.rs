use std::collections::HashMap;

lazy_static::lazy_static! {
    pub static ref T50_MAP: CharEncodingMap = CharEncodingMap::from_map_file(include_str!("T50.map"));
    pub static ref T61_MAP: CharEncodingMap = CharEncodingMap::from_map_file(include_str!("T61.map"));
    pub static ref T100_MAP: CharEncodingMap = CharEncodingMap::from_map_file(include_str!("T100.map"));
}

pub struct CharEncodingMap {
    byte_to_char: HashMap<u8, char>,
    char_to_byte: HashMap<char, u8>,
}

impl CharEncodingMap {
    pub fn from_map_file(map_file: &str) -> CharEncodingMap {
        let lines = map_file.split("\n").collect::<Vec<&str>>();
        let mut byte_to_char = HashMap::with_capacity(lines.len());
        let mut char_to_byte = HashMap::with_capacity(lines.len());

        for line in lines {
            if line.is_empty() {
                continue;
            }
            if line.chars().skip(2).next().unwrap() != '=' {
                panic!("malformed map entry: {}", line);
            }
            let byte = u8::from_str_radix(&line[..2], 16).expect("invalid hex byte");
            let char_data = &line[3..];
            let char = if char_data.chars().count() > 1 {
                // control character
                if char_data == "SP" {
                    // space is SP in the map file to make it more clear
                    // since it's not actually a control character, change it to its char representation
                    ' '
                } else {
                    // control characters are not preserved;
                    // they are encoded as the raw byte value
                    byte as char
                }
            } else {
                char_data.chars().next().unwrap()
            };

            byte_to_char.insert(byte, char);
            char_to_byte.insert(char, byte);
        }

        CharEncodingMap {
            byte_to_char,
            char_to_byte,
        }
    }

    pub fn encode_char(&self, ch: char) -> Option<u8> {
        self.char_to_byte.get(&ch).cloned()
    }

    pub fn decode_byte(&self, byte: u8) -> Option<char> {
        self.byte_to_char.get(&byte).cloned()
    }

    pub fn encode_str(&self, s: &str) -> Option<Vec<u8>> {
        let mut bytes = Vec::new();
        for ch in s.chars() {
            match self.encode_char(ch) {
                Some(byte) => bytes.push(byte),
                None => return None,
            }
        }
        Some(bytes)
    }

    pub fn decode_bytes(&self, bytes: &[u8]) -> Option<String> {
        let mut str = String::with_capacity(bytes.len());
        for byte in bytes {
            match self.decode_byte(*byte) {
                Some(ch) => str.push(ch),
                None => return None,
            }
        }
        Some(str)
    }

    pub fn contains_char(&self, ch: char) -> bool {
        self.char_to_byte.contains_key(&ch)
    }
}
