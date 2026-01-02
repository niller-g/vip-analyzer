/// Decode a byte slice into a string, first trying to interpret it as UTF-8, otherwise as UTF-16.
///
/// The `bytes` are allowed to have a BOM (byte order mark) at the beginning.
/// If there is no BOM, endianness for UTF-16 will be guessed.
///
/// If the endianness cannot be guessed, the `fallback_to_little_endian` parameter
/// will determine the endianness to use. `fallback_to_little_endian = true` will use little-endian,
/// `fallback_to_little_endian = false` will use big-endian.
///
/// Returns an error if the input is not a valid UTF-8/UTF-16 sequence.
pub fn to_utf8_else_utf16(
    bytes: &[u8],
    fallback_to_little_endian: bool,
) -> Result<std::borrow::Cow<'_, str>, &'static str> {
    match std::str::from_utf8(bytes) {
        Ok(s) => Ok(std::borrow::Cow::Borrowed(s.trim_start_matches("\u{FEFF}"))), // Strip BOM if it exists.
        Err(_) => match to_utf16_bytes(bytes, fallback_to_little_endian) {
            Some(utf16) => match String::from_utf16(&utf16) {
                Ok(s) => Ok(std::borrow::Cow::Owned(s)),
                Err(_) => Err("Invalid UTF-16 sequence"),
            },
            None => Err("Invalid UTF-8/UTF-16 byte sequence"),
        },
    }
}

/// Convert a slice of `u8` into the UTF-16 interpretation.
/// The input `bytes` should have a beginning BOM (byte order mark), otherwise it will be guessed.
///
/// If the endianness cannot be guessed, the `fallback_to_little_endian` parameter
/// will determine the endianness to use. `fallback_to_little_endian = true` will use little-endian,
/// `fallback_to_little_endian = false` will use big-endian.
///
/// Returns `None` if the input is not a valid UTF-16 sequence.
fn to_utf16_bytes(bytes: &[u8], fallback_to_little_endian: bool) -> Option<Vec<u16>> {
    let len = bytes.len();
    if !len.is_multiple_of(2) {
        return None;
    }

    let (little_endian, bytes) = if len >= 2 {
        match bytes[..2] {
            [0xFF, 0xFE] => (true, &bytes[2..]),
            [0xFE, 0xFF] => (false, &bytes[2..]),
            [b, 0x00] if b != 0x00 => (true, bytes),
            [0x00, b] if b != 0x00 => (false, bytes),
            _ => (fallback_to_little_endian, bytes),
        }
    } else {
        (fallback_to_little_endian, bytes)
    };

    let convert = if little_endian { u16::from_le_bytes } else { u16::from_be_bytes };
    Some(bytes.chunks(2).map(|chunk| convert([chunk[0], chunk[1]])).collect())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn decode_utf16_guess_endianness() {
        let bytes = vec![
            0x48, 0x00, // 'H'
            0x65, 0x00, // 'e'
            0x6C, 0x00, // 'l'
            0x6C, 0x00, // 'l'
            0x6F, 0x00, // 'o'
        ];
        assert_eq!(String::from_utf16(&to_utf16_bytes(&bytes, true).unwrap()).unwrap(), "Hello");
        assert_eq!(String::from_utf16(&to_utf16_bytes(&bytes, false).unwrap()).unwrap(), "Hello");

        let bytes = vec![
            0x00, 0x48, // 'H'
            0x00, 0x65, // 'e'
            0x00, 0x6C, // 'l'
            0x00, 0x6C, // 'l'
            0x00, 0x6F, // 'o'
        ];

        assert_eq!(String::from_utf16(&to_utf16_bytes(&bytes, true).unwrap()).unwrap(), "Hello");
        assert_eq!(String::from_utf16(&to_utf16_bytes(&bytes, false).unwrap()).unwrap(), "Hello");
    }

    #[test]
    fn decode_utf16_fallback_endianness() {
        let bytes = vec![
            0x48, 0x48, // '䡈'
            0x65, 0x00, // 'e'
            0x6C, 0x00, // 'l'
            0x6C, 0x00, // 'l'
            0x6F, 0x00, // 'o'
        ];
        assert_eq!(String::from_utf16(&to_utf16_bytes(&bytes, true).unwrap()).unwrap(), "䡈ello");
        assert_eq!(
            String::from_utf16(&to_utf16_bytes(&bytes, false).unwrap()).unwrap(),
            "䡈攀氀氀漀"
        );
    }

    #[test]
    fn decode_utf16_little_endian() {
        let bytes = vec![
            0xFF, 0xFE, // BOM: UTF-16 little-endian
            0x48, 0x00, // 'H'
            0x65, 0x00, // 'e'
            0x6C, 0x00, // 'l'
            0x6C, 0x00, // 'l'
            0x6F, 0x00, // 'o'
        ];
        assert_eq!(String::from_utf16(&to_utf16_bytes(&bytes, true).unwrap()).unwrap(), "Hello");
        assert_eq!(String::from_utf16(&to_utf16_bytes(&bytes, false).unwrap()).unwrap(), "Hello");
    }

    #[test]
    fn decode_utf16_big_endian() {
        let bytes = vec![
            0xFE, 0xFF, // BOM: UTF-16 big-endian
            0x00, 0x48, // 'H'
            0x00, 0x65, // 'e'
            0x00, 0x6C, // 'l'
            0x00, 0x6C, // 'l'
            0x00, 0x6F, // 'o'
        ];
        assert_eq!(String::from_utf16(&to_utf16_bytes(&bytes, true).unwrap()).unwrap(), "Hello");
        assert_eq!(String::from_utf16(&to_utf16_bytes(&bytes, false).unwrap()).unwrap(), "Hello");
    }
}
