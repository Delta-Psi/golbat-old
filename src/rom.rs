pub struct Rom {
    pub header: Header,
    pub data: Vec<u8>,
}

#[derive(Debug)]
pub struct Header {
    pub entry_point: Vec<u8>, // 4 bytes
    pub nintendo_logo: Vec<u8>, // 48 bytes
    pub game_title: String,
    pub cgb: bool,
    pub licensee_code: LicenseeCode,
    pub sgb: bool,
    pub cartridge_type: u8,
    pub rom_size: u8,
    pub ram_size: u8,
    pub destination_code: u8,
    pub version: u8,
    pub complement_check: u8,
    pub checksum: u16,
}

#[derive(Debug)]
pub enum LicenseeCode {
    Accolade,
    Konami,
    Other(u8),
}

use std::io::{self, Read};

pub enum LoadError {
    IoError(io::Error),
    HeaderParseError(HeaderParseError),
}

impl Rom {
    pub fn read<R: Read>(reader: R) -> Result<Rom, LoadError> {
        let mut buffer = Vec::new();
        reader.read_to_end(&mut buffer).map_err(|err| LoadError::IoError(err))?;

        Ok(Rom {
            header: Header::parse(&buffer[0x0100 .. 0x0150])
                .map_err(|err| LoadError::HeaderParseError(err))?,
            data: buffer,
        })
    }
}

use nom;
use std::string::FromUtf8Error;

pub enum HeaderParseError {
    NomError(nom::IError),
    InvalidGameTitle(FromUtf8Error),
    InvalidCgbFlag(u8),
    InvalidLicenseeCode(u8, [u8; 2]),
    InvalidSgbFlag(u8),
    InvalidCartridgeType(u8),
    InvalidRomSize(u8),
    InvalidRamSize(u8),
    InvalidDestinationCode(u8),
}

impl Header {
    pub fn parse(data: &[u8]) -> Result<Header, HeaderParseError> {
        let header = parse::header(data).to_result().map_err(
            |e| HeaderParseError::NomError(nom::IError::Error(e))
        )?;

        Ok(Header {
            entry_point: header.entry_point.to_vec(),
            nintendo_logo: header.nintendo_logo.to_vec(),
            game_title: match String::from_utf8(
                    header.game_title.splitn(2, |b| *b == 0).nth(0).unwrap().to_vec()
                ) {
                Ok(title) => title,
                Err(err) => return Err(HeaderParseError::InvalidGameTitle(err)),
            },
            cgb: match header.cgb {
                0x80 => true,
                0x00 => false,
                f => return Err(HeaderParseError::InvalidCgbFlag(f)),
            },
            licensee_code: match header.licensee_code2 {
                0x33 => LicenseeCode::Other(header.licensee_code1
        })
    }
}

mod parse {
    use nom::*;

    pub struct Header<'a> {
        pub entry_point: &'a [u8],
        pub nintendo_logo: &'a [u8],
        pub game_title: &'a [u8],
        pub cgb: u8,
        pub licensee_code1: &'a [u8],
        pub sgb: u8,
        pub cartridge_type: u8,
        pub rom_size: u8,
        pub ram_size: u8,
        pub destination_code: u8,
        pub licensee_code2: u8,
        pub version: u8,
        pub complement_check: u8,
        pub checksum: u16,
    }

    named!(pub header<&[u8], Header>, do_parse!(
        entry_point:        take!(4) >>
        nintendo_logo:      take!(48) >>
        game_title:         take!(16) >>
        cgb:                le_u8 >>
        licensee_code1:     take!(2) >>
        sgb:                le_u8 >>
        cartridge_type:     le_u8 >>
        rom_size:           le_u8 >>
        ram_size:           le_u8 >>
        destination_code:   le_u8 >>
        licensee_code2:     le_u8 >>
        version:            le_u8 >>
        complement_check:   le_u8 >>
        checksum:           le_u16 >>
        (Header {
            entry_point,
            nintendo_logo,
            game_title,
            cgb,
            licensee_code1,
            sgb,
            cartridge_type,
            rom_size,
            ram_size,
            destination_code,
            licensee_code2,
            version,
            complement_check,
            checksum,
        })
));
}


