use std::error::Error;
use std::fs::File;
use std::io::prelude::*;
use std::path::Path;

#[repr(packed)]
struct Dump {
    a : i8,
    b : u8,
    c : i16,
    d : u16,
    e : i32,
    f : u32,
    g : [u8; 8],
    h : [u8; 8]
}

unsafe fn any_as_u8_slice<T: Sized>(p: &T) -> &[u8] {
    ::std::slice::from_raw_parts(
        (p as *const T) as *const u8,
        ::std::mem::size_of::<T>(),
    )
}

fn main() {
    let d = Dump {
        a: -100,
        b: 200,
        c: -32000,
        d: 33000,
        e: -2000000000,
        f: 3000000000,
        g: *b"namename",
        h: *b"name\0\0\0\0"
    };

    let bs = unsafe {any_as_u8_slice(&d)};

    println!("{}", bs.len());
    let mut o = File::create("dmp").unwrap();
    o.write_all(bs).unwrap();
}