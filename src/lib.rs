#![feature(macro_rules, struct_variant, phase)]

extern crate serialize;
#[phase(plugin, link)] extern crate log;

use std::io::{IoError, MemWriter, MemReader};
use std::raw::Slice as RawSlice;
use std::mem;
use std::str;
use serialize::{Encodable, Decodable};

static SOME: u8 = 0x80;
static NONE: u8 = 0x81;
static ESCAPE: u8 = 0x82;
static END: u8 = 0x83;

pub struct Encoder<'a> {
    writer: &'a mut Writer + 'a,
}

impl<'a> Encoder<'a> {
    pub fn new(writer: &mut Writer) -> Encoder {
        Encoder { writer: writer }
    }

    pub fn buffer_encode<T>(object: &T) -> Vec<u8>
      where T: Encodable<Encoder<'a>, IoError> {
        let mut m = MemWriter::new();
        unsafe {
            let mut encoder = Encoder::new(&mut m as &mut Writer);
            let _ = object.encode(mem::transmute(&mut encoder));
        }
        m.unwrap()
    }
}

type EncodeResult = Result<(), IoError>;

// TODO: endianness and whatnot
fn as_bytes<T>(x: &T) -> &[u8] {
    let slice = RawSlice { data: x, len: mem::size_of::<T>() };
    unsafe { mem::transmute(slice) }
}

impl<'a> serialize::Encoder<IoError> for Encoder<'a> {
    fn emit_nil(&mut self) -> EncodeResult { self.writer.write(&[0]) }

    fn emit_uint(&mut self, v: uint) -> EncodeResult {
        self.emit_u64(v as u64)
    }

    fn emit_u64(&mut self, v: u64) -> EncodeResult {
        self.writer.write(as_bytes(&v))
    }

    fn emit_u32(&mut self, v: u32) -> EncodeResult {
        self.writer.write(as_bytes(&v))
    }

    fn emit_u16(&mut self, v: u16) -> EncodeResult {
        self.writer.write(as_bytes(&v))
    }

    fn emit_u8(&mut self, v: u8) -> EncodeResult {
        self.writer.write(as_bytes(&v))
    }

    fn emit_int(&mut self, v: int) -> EncodeResult {
        self.emit_i64(v as i64)
    }

    fn emit_i64(&mut self, v: i64) -> EncodeResult {
        self.writer.write(as_bytes(&v))
    }

    fn emit_i32(&mut self, v: i32) -> EncodeResult {
        self.writer.write(as_bytes(&v))
    }

    fn emit_i16(&mut self, v: i16) -> EncodeResult {
        self.writer.write(as_bytes(&v))
    }

    fn emit_i8(&mut self, v: i8) -> EncodeResult {
        self.writer.write(as_bytes(&v))
    }

    fn emit_bool(&mut self, v: bool) -> EncodeResult {
        self.writer.write(&[v as u8])
    }

    fn emit_f64(&mut self, v: f64) -> EncodeResult {
        self.writer.write(as_bytes(&v))
    }

    fn emit_f32(&mut self, v: f32) -> EncodeResult {
        self.writer.write(as_bytes(&v))
    }

    fn emit_char(&mut self, v: char) -> EncodeResult {
        self.writer.write(as_bytes(&v))
    }

    fn emit_str(&mut self, v: &str) -> EncodeResult {
        try!(self.writer.write(v.bytes().flat_map(|x|
            if x == END || x == ESCAPE { (vec![ESCAPE, x]).move_iter() }
            else { (vec![x]).move_iter() }
        ).collect::<Vec<u8>>().as_slice()));
        self.writer.write(&[END])
    }

    fn emit_enum(&mut self, _name: &str, f: |&mut Encoder<'a>| -> EncodeResult) -> EncodeResult {
        f(self)
        // end?
    }

    fn emit_enum_variant(&mut self, name: &str, _id: uint, _cnt: uint,
                         f: |&mut Encoder<'a>| -> EncodeResult) -> EncodeResult {
        try!(self.emit_str(name));
        try!(f(self));
        self.writer.write(&[END])
    }

    fn emit_enum_variant_arg(&mut self, _idx: uint,
                            f: |&mut Encoder<'a>| -> EncodeResult) -> EncodeResult {
        f(self)
        // end?
    }

    fn emit_enum_struct_variant(&mut self, name: &str, id: uint, cnt: uint,
                                f: |&mut Encoder<'a>| -> EncodeResult) -> EncodeResult {
        self.emit_enum_variant(name, id, cnt, f)
    }

    fn emit_enum_struct_variant_field(&mut self, _name: &str, idx: uint,
                                      f: |&mut Encoder<'a>| -> EncodeResult) -> EncodeResult {
        self.emit_enum_variant_arg(idx, f)
    }

    fn emit_struct(&mut self,
                   _: &str,
                   _: uint,
                   f: |&mut Encoder<'a>| -> EncodeResult) -> EncodeResult {
        f(self)
        // end?
    }

    fn emit_struct_field(&mut self,
                         name: &str,
                         _idx: uint,
                         f: |&mut Encoder<'a>| -> EncodeResult) -> EncodeResult {
        try!(self.emit_str(name));
        f(self)
    }

    fn emit_tuple(&mut self, len: uint, f: |&mut Encoder<'a>| -> EncodeResult) -> EncodeResult {
        self.emit_seq(len, f)
    }

    fn emit_tuple_arg(&mut self,
                      idx: uint,
                      f: |&mut Encoder<'a>| -> EncodeResult) -> EncodeResult {
        self.emit_seq_elt(idx, f)
    }

    fn emit_tuple_struct(&mut self,
                         _name: &str,
                         len: uint,
                         f: |&mut Encoder<'a>| -> EncodeResult) -> EncodeResult {
        self.emit_seq(len, f)
    }

    fn emit_tuple_struct_arg(&mut self,
                             idx: uint,
                             f: |&mut Encoder<'a>| -> EncodeResult) -> EncodeResult {
        self.emit_seq_elt(idx, f)
    }

    fn emit_option(&mut self, f: |&mut Encoder<'a>| -> EncodeResult) -> EncodeResult {
        f(self)
        // end?
    }

    fn emit_option_none(&mut self) -> EncodeResult { self.writer.write(&[NONE]) }

    fn emit_option_some(&mut self, f: |&mut Encoder<'a>| -> EncodeResult) -> EncodeResult {
        try!(self.writer.write(&[SOME]));
        f(self)
        // end?
    }

    fn emit_seq(&mut self, len: uint, f: |&mut Encoder<'a>| -> EncodeResult) -> EncodeResult {
        try!(self.emit_uint(len));
        f(self)
        //self.writer.write(&[END])
    }

    fn emit_seq_elt(&mut self, _idx: uint, f: |&mut Encoder<'a>| -> EncodeResult) -> EncodeResult {
        f(self)
        // end?
    }

    fn emit_map(&mut self, len: uint, f: |&mut Encoder<'a>| -> EncodeResult) -> EncodeResult {
        try!(self.emit_uint(len));
        f(self)
        //self.writer.write(&[END])
    }

    fn emit_map_elt_key(&mut self,
                        _idx: uint,
                        f: |&mut Encoder<'a>| -> EncodeResult) -> EncodeResult {
        f(self)
        // end?
    }

    fn emit_map_elt_val(&mut self,
                        _idx: uint,
                        f: |&mut Encoder<'a>| -> EncodeResult) -> EncodeResult {
        f(self)
        // end?
    }
}

pub struct Decoder<'a> {
    reader: &'a mut Reader + 'a
}

impl<'a> Decoder<'a> {
    pub fn new(reader: &mut Reader) -> Decoder {
        Decoder { reader: reader }
    }

    pub fn buffer_decode<T>(v: Vec<u8>) -> DecodeResult<T>
      where T: Decodable<Decoder<'a>, DecodeError> {
        let mut reader = MemReader::new(v);
        let mut decoder = Decoder::new(unsafe { mem::transmute(&mut reader as &mut Reader) });

        Decodable::decode(&mut decoder)
    }
}

pub type DecodeResult<T> = Result<T, DecodeError>;

#[deriving(Show, Clone, PartialEq, Eq)]
pub enum DecodeError {
    IoError(IoError),
    UnknownVariantError(String),
    InvalidUtf8Error,
    ExpectedError(u8),
    OtherError(String),
}

// TODO: endianness and whatnot
fn from_bytes<T>(slice: &[u8]) -> &T {
    let x: RawSlice<T> = unsafe { mem::transmute(slice) };
    unsafe { mem::transmute(x.data) }
}

macro_rules! prim_impl {
    ($n:ident, $t: ty, $size: expr) => {
        fn $n(&mut self) -> DecodeResult<$t> {
            Ok(*from_bytes(try!(self.reader.read_exact($size).map_err(|x| IoError(x))).as_slice()))
        }
    }
}

impl<'a> serialize::Decoder<DecodeError> for Decoder<'a> {
    fn read_nil(&mut self) -> DecodeResult<()> { Ok(()) }

    fn read_uint(&mut self) -> DecodeResult<uint> {
        Ok(try!(self.read_u64()) as uint)
    }

    prim_impl!(read_u64, u64, 8)
    prim_impl!(read_u32, u32, 4)
    prim_impl!(read_u16, u16, 2)
    prim_impl!(read_u8,  u8,  1)

    fn read_int(&mut self) -> DecodeResult<int> {
        Ok(try!(self.read_i64()) as int)
    }

    prim_impl!(read_i64, i64, 8)
    prim_impl!(read_i32, i32, 4)
    prim_impl!(read_i16, i16, 2)
    prim_impl!(read_i8,  i8,  1)

    fn read_bool(&mut self) -> DecodeResult<bool> {
        Ok(try!(self.read_u8()) != 0)
    }

    prim_impl!(read_f64,  f64,  8)
    prim_impl!(read_f32,  f32,  4)
    prim_impl!(read_char, char, 8)

    fn read_str(&mut self) -> DecodeResult<String> {
        debug!("read_str");
        let mut buf = Vec::new();
        loop {
            let b = self.reader.read_byte();
            match b {
                Err(e) => return Err(IoError(e)),
                Ok(ESCAPE) => buf.push(try!(self.reader.read_byte().map_err(|x| IoError(x)))),
                Ok(END) => break,
                Ok(a) => buf.push(a),
            }
        }
        match String::from_utf8(buf) {
            Ok(v) => Ok(v),
            Err(_) => Err(InvalidUtf8Error),
        }
    }

    fn read_enum<T>(&mut self, _name: &str,
                    f: |&mut Decoder<'a>| -> DecodeResult<T>) -> DecodeResult<T> {
        f(self)
    }

    fn read_enum_variant<T>(&mut self,
                            names: &[&str],
                            f: |&mut Decoder<'a>, uint| -> DecodeResult<T>)
                            -> DecodeResult<T> {
        let name = try!(self.read_str());
        let idx = match names.iter().position(|n| str::eq_slice(*n, name.as_slice())) {
            Some(idx) => idx,
            None => return Err(UnknownVariantError(name)),
        };
        let r = try!(f(self, idx));
        if try!(self.reader.read_byte().map_err(|x| IoError(x))) != END {
            return Err(ExpectedError(END));
        }
        Ok(r)
    }

    fn read_enum_variant_arg<T>(&mut self, _idx: uint, f: |&mut Decoder<'a>| -> DecodeResult<T>)
                                -> DecodeResult<T> {
        f(self)
    }

    fn read_enum_struct_variant<T>(&mut self,
                                   names: &[&str],
                                   f: |&mut Decoder<'a>, uint| -> DecodeResult<T>)
                                   -> DecodeResult<T> {
        self.read_enum_variant(names, f)
    }

    fn read_enum_struct_variant_field<T>(&mut self,
                                         _name: &str,
                                         idx: uint,
                                         f: |&mut Decoder<'a>| -> DecodeResult<T>)
                                         -> DecodeResult<T> {
        self.read_enum_variant_arg(idx, f)
    }

    fn read_struct<T>(&mut self,
                      _name: &str,
                      _len: uint,
                      f: |&mut Decoder<'a>| -> DecodeResult<T>)
                      -> DecodeResult<T> {
        debug!("read_struct({}, {})", _name, _len);
        f(self)
    }

    fn read_struct_field<T>(&mut self,
                            _name: &str,
                            _idx: uint,
                            f: |&mut Decoder<'a>| -> DecodeResult<T>)
                            -> DecodeResult<T> {
        debug!("read_struct_field({}, {})", _name, _idx);
        // just be lazy and ignore the name
        try!(self.read_str());
        f(self)
    }

    fn read_tuple<T>(&mut self, f: |&mut Decoder<'a>, uint| -> DecodeResult<T>) -> DecodeResult<T> {
        self.read_seq(f)
    }

    fn read_tuple_arg<T>(&mut self,
                         idx: uint,
                         f: |&mut Decoder<'a>| -> DecodeResult<T>) -> DecodeResult<T> {
        self.read_seq_elt(idx, f)
    }

    fn read_tuple_struct<T>(&mut self,
                            _name: &str,
                            f: |&mut Decoder<'a>, uint| -> DecodeResult<T>)
                            -> DecodeResult<T> {
        self.read_tuple(f)
    }

    fn read_tuple_struct_arg<T>(&mut self,
                                idx: uint,
                                f: |&mut Decoder<'a>| -> DecodeResult<T>)
                                -> DecodeResult<T> {
        self.read_tuple_arg(idx, f)
    }

    fn read_option<T>(&mut self, f: |&mut Decoder<'a>, bool| -> DecodeResult<T>) -> DecodeResult<T> {
        match try!(self.reader.read_byte().map_err(|x| IoError(x))) {
            NONE => f(self, false),
            SOME => f(self, true), // is this right?
            _ => Err(UnknownVariantError("".to_string())),
        }
    }

    fn read_seq<T>(&mut self, f: |&mut Decoder<'a>, uint| -> DecodeResult<T>) -> DecodeResult<T> {
        let len = try!(self.read_uint());
        f(self, len)
    }

    fn read_seq_elt<T>(&mut self,
                       _idx: uint,
                       f: |&mut Decoder<'a>| -> DecodeResult<T>) -> DecodeResult<T> {
        f(self)
    }

    fn read_map<T>(&mut self, f: |&mut Decoder<'a>, uint| -> DecodeResult<T>) -> DecodeResult<T> {
        let len = try!(self.read_uint());
        f(self, len)
    }

    fn read_map_elt_key<T>(&mut self,
                           idx: uint,
                           f: |&mut Decoder<'a>| -> DecodeResult<T>) -> DecodeResult<T> {
        self.read_seq_elt(idx, f)
    }

    fn read_map_elt_val<T>(&mut self,
                           idx: uint,
                           f: |&mut Decoder<'a>| -> DecodeResult<T>) -> DecodeResult<T> {
        self.read_seq_elt(idx, f)
    }

    fn error(&mut self, err: &str) -> DecodeError {
        OtherError(err.to_string())
    }
}

#[cfg(test)]
mod test {
    use super::{Encoder, Decoder, DecodeResult};
    use std::collections::HashMap;

    #[test]
    fn cycle_maps() {
        let mut x = HashMap::new();
        x.insert(vec![1i, 2i], "hello world".to_string());
        x.insert(vec![5i, -3i], "potato".to_string());
        let enc = Encoder::buffer_encode(&x);
        println!("From: {}", enc);

        let dec: DecodeResult<HashMap<Vec<int>, String>> = Decoder::buffer_decode(enc);
        println!("To:   {}", dec);

        assert_eq!(dec, Ok(x));
    }

    #[test]
    fn cycle_enum() {
        #[deriving(Encodable, Decodable, PartialEq, Show)]
        enum Foo {
            Bar(int),
            Baz { x: int, y: f64 },
            Qux(String, int),
        }

        let x = Qux("hello".to_string(), 23478);
        let enc = Encoder::buffer_encode(&x);
        println!("From: {}", enc);

        let dec: DecodeResult<Foo> = Decoder::buffer_decode(enc);
        println!("To:   {}", dec);

        assert_eq!(dec, Ok(x));
    }

    #[test]
    fn cycle_struct() {
        #[deriving(Encodable, Decodable, PartialEq, Show)]
        struct Foo {
            x: String,
            y: (int, int),
        }

        let x = Foo { x: "hello \x83 world".to_string(), y: (4656, -12313) };
        let enc = Encoder::buffer_encode(&x);
        println!("From: {}", enc);
        println!("Str:  {}", String::from_utf8_lossy(enc.as_slice()));

        let dec: DecodeResult<Foo> = Decoder::buffer_decode(enc);
        println!("To:   {}", dec);

        assert_eq!(dec, Ok(x));
    }
}
