#[derive(Debug)]
pub struct Rom {
    pub data: Vec<u8>,
    pub header: Header,
}

impl Rom {
    pub fn read(data: &[u8]) -> Result<Rom, HeaderParseError> {
        Ok(Rom {
            data: data.to_vec(),
            header: Header::parse(&data[0x0100..])?,
        })
    }

    /*
    pub fn check_header_complement(&self) -> (bool, u8) {
        let mut acc: u8 = 0;
        for (b, i) in self.data[0x0134 .. 0x014c].iter().zip(0x0134usize ..) {
            if i != 0x014d {
                acc = acc.wrapping_sub(*b as u8 + 1);
            }
        }

        (acc == self.header.complement_check, acc)
    }

    pub fn check_checksum(&self) -> (bool, u16) {
        let mut acc: u16 = 0;
        for (i, b) in self.data.iter().enumerate() {
            if i != 0x014e && i != 0x014f {
                acc = acc.wrapping_add(*b as u16);
            }
        }

        (acc == self.header.checksum, acc)
    }
    FIXME
    */
}

#[derive(Debug)]
pub struct Header {
    pub entry_point: Vec<u8>,
    pub nintendo_logo: Vec<u8>,
    pub game_title: String,
    pub cgb: bool,
    // TODO: licensee code
    pub sgb: bool,
    pub cartrige_type: u8, // TODO: proper parsing here
    pub rom_size: u8, // same here
    pub ram_size: u8, // and here
    pub destination_code: u8,
    // old licensee code would go here...
    pub version_number: u8,
    pub complement_check: u8,
    pub checksum: u16,
}

use nom::Err;

#[derive(Debug)]
pub enum HeaderParseError<'a> {
    NomError(Err<&'a [u8]>),
}

impl Header {
    pub fn parse(data: &[u8]) -> Result<Header, HeaderParseError> {
        let (_, header) = parse::header(data).map_err(|e| HeaderParseError::NomError(e))?;
        Ok(Header {
            entry_point: header.entry_point,
            nintendo_logo: header.nintendo_logo,
            // remove padding
            game_title: {
                let title = header.game_title.split(|b| *b == 0).nth(0).unwrap();
                String::from_utf8_lossy(&title).into_owned()
            },
            cgb: header.cgb_flag == 0x80,
            // TODO: licensee code?
            sgb: header.sgb_flag == 0x03,
            cartrige_type: header.cartridge_type,
            rom_size: header.rom_size,
            ram_size: header.ram_size,
            destination_code: header.destination_code,
            version_number: header.version_number,
            complement_check: header.complement_check,
            checksum: header.checksum,
        })
    }
}

mod parse {
    use nom::*;

    #[derive(Debug)]
    pub struct Header {
        pub entry_point: Vec<u8>, // 4 bytes
        pub nintendo_logo: Vec<u8>, // 48 bytes
        pub game_title: Vec<u8>, // 16 bytes
        pub cgb_flag: u8,
        pub licensee_code1: Vec<u8>, // 2 bytes
        pub sgb_flag: u8,
        pub cartridge_type: u8,
        pub rom_size: u8,
        pub ram_size: u8,
        pub destination_code: u8,
        pub licensee_code2: u8,
        pub version_number: u8,
        pub complement_check: u8,
        pub checksum: u16,
    }

    named!(pub header<&[u8], Header>,
           do_parse!(
               entry_point:         take!(4)    >>
               nintendo_logo:       take!(48)   >>
               game_title:          take!(16)   >>
               cgb_flag:            le_u8       >>
               licensee_code1:      take!(2)    >>
               sgb_flag:            le_u8       >>
               cartridge_type:      le_u8       >>
               rom_size:            le_u8       >>
               ram_size:            le_u8       >>
               destination_code:    le_u8       >>
               licensee_code2:      le_u8       >>
               version_number:      le_u8       >>
               complement_check:    le_u8       >>
               checksum:            le_u16      >>
               (Header {
                   entry_point: entry_point.to_vec(),
                   nintendo_logo: nintendo_logo.to_vec(),
                   game_title: game_title.to_vec(),
                   cgb_flag,
                   licensee_code1: licensee_code1.to_vec(),
                   sgb_flag,
                   cartridge_type,
                   rom_size,
                   ram_size,
                   destination_code,
                   licensee_code2,
                   version_number,
                   complement_check,
                   checksum,
               })
          )
    );
}
