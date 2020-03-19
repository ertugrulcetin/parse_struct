use std::fs::File;
use std::io::Write;

#[derive(Debug, Clone, Copy)]
#[repr(packed)]
struct S1 {
    a: i8,
    b: u8,
    c: i16,
    d: u16,
    e: i32,
    f: u32,
    g: [u8; 8],
    h: [u8; 8]
}

#[derive(Debug, Clone, Copy)]
#[repr(packed)]
struct Empty {}

#[derive(Debug, Clone, Copy)]
#[repr(packed)]
struct S2 {
    a: i32,
    b: Empty,
    c: [u8; 6]
}

#[derive(Debug, Clone, Copy)]
#[repr(packed)]
struct S3 {
    a: i32,
    b: S2
}

#[derive(Debug, Clone, Copy)]
#[repr(packed)]
struct S4 {
    a: u8,
    b: [S2; 3]
}

unsafe fn any_as_u8_slice<T: Sized>(p: &T) -> &[u8] {
    ::std::slice::from_raw_parts(
        (p as *const T) as *const u8,
        ::std::mem::size_of::<T>(),
    )
}

fn dump_to_file<T: Sized>(t: &T, fl: &str) {
    let bs = unsafe { any_as_u8_slice(t) };
    let mut f = File::create(fl).unwrap();
    f.write_all(bs);
}

fn main() {
    let s1 = S1 {
        a: -100,
        b: 200,
        c: -32000,
        d: 33000,
        e: -2100000000,
        f: 2200000000,
        g: *b"name\0\0\0\0",
        h: *b"namefull"
    };
    dump_to_file(&s1, "test/data/dmp1");

    let arr = [s1; 20];
    dump_to_file(&arr, "test/data/dmp2");

    let s2 = S2 {
        a: 3000,
        b: Empty {},
        c: *b"myname"
    };
    dump_to_file(&s2, "test/data/dmp3");

    let ints = [450; 10];
    dump_to_file(&ints, "test/data/dmp4");

    let grid = [[5; 10]; 20];
    dump_to_file(&grid, "test/data/dmp5");

    let s3 = S3 {
        a: -45,
        b: S2 {
            a: 0,
            b: Empty {},
            c: *b"here\0\0"
        }
    };
    dump_to_file(&s3, "test/data/dmp6");

    let s4 = S4 {
        a: 200,
        b: [S2 {
            a: -5,
            b: Empty {},
            c: *b"anothe"
        }; 3]
    };
    dump_to_file(&s4, "test/data/dmp7");
}
