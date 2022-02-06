use std::{ptr, slice};
use std::cmp::Ordering;
use std::marker::PhantomData;
use std::ops::{Range, RangeInclusive};

use crate::symbols::{SymbolMap, Symbol};

/// Translation phase 3 - whitespace and comments, UCNs, and preprocessing tokens
#[derive(Copy, Clone)]
pub struct Tokens<'s> { ptr: *const u8, end: *const u8, _marker: PhantomData<&'s [u8]> }

#[derive(Copy, Clone)]
pub struct Space { pub kind: Shape, pub len: usize }

#[derive(Copy, Clone, PartialEq, Eq)]
pub enum Shape { None, Horizontal, Newline }

/// A preprocessing token.
#[derive(Copy, Clone)]
pub struct Token<'i, 's> { kind: Kind, spelling: Spelling<'i, 's>, len: usize }

#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub enum Kind {
    EndOfFile, EndOfLine,

    HeaderName,

    Identifier, Number, Character, String,

    Hash, HashHash,

    LeftBrace, RightBrace, LeftBracket, RightBracket, LeftParen, RightParen,
    Semi, Colon, Ellipsis,
    Question, ColonColon, Dot, DotStar, Arrow, ArrowStar, Tilde,
    Exclaim, Plus, Minus, Star, Slash, Percent, Caret, Amp, Pipe,
    Eq, PlusEq, MinusEq, StarEq, SlashEq, PercentEq, CaretEq, AmpEq, PipeEq,
    EqEq, ExclaimEq, Lt, Gt, LtEq, GtEq, LtEqGt, AmpAmp, PipePipe,
    LtLt, GtGt, LtLtEq, GtGtEq, PlusPlus, MinusMinus, Comma,

    Error,
}

/// A preprocessing token's *spelling*, used for stringizing and pasting.
///
/// This may already point to an interned symbol, or into the source buffer where it may still
/// contain splices or UCNs to be removed.
#[derive(Copy, Clone)]
enum Spelling<'i, 's> {
    Intern(Symbol<'i, Kind>),
    Buffer { flags: u8, data: ptr::NonNull<u8>, _marker: PhantomData<&'s [u8]> },
}

const FLAG_SPLICE: u8 = 1 << 0;
const FLAG_UCN: u8 = 1 << 1;

impl<'s> Tokens<'s> {
    pub fn from_bytes_with_padding(bytes: &'s [u8]) -> Option<Tokens<'s>> {
        match *bytes {
            [.., 0, 0, 0] => {
                let Range { start, end } = bytes.as_ptr_range();
                let ptr = start;
                let end = unsafe { end.offset(-2) };
                Some(Tokens { ptr, end, _marker: PhantomData })
            }
            _ => { None }
        }
    }

    /// Assign kinds to alternative token spellings.
    ///
    /// Should be called before interning anything else that might overlap.
    pub fn alternative_tokens(symbols: &SymbolMap<Kind>) {
        symbols.intern(b"and", Kind::AmpAmp);
        symbols.intern(b"and_eq", Kind::AmpEq);
        symbols.intern(b"bitand", Kind::Amp);
        symbols.intern(b"bitor", Kind::Pipe);
        symbols.intern(b"compl", Kind::Tilde);
        symbols.intern(b"not", Kind::Exclaim);
        symbols.intern(b"not_eq", Kind::ExclaimEq);
        symbols.intern(b"or", Kind::PipePipe);
        symbols.intern(b"or_eq", Kind::PipeEq);
        symbols.intern(b"xor", Kind::Caret);
        symbols.intern(b"xor_eq", Kind::CaretEq);
    }

    /// Consume whitespace and comments.
    pub fn whitespace(&mut self, horizontal: bool) -> Space {
        unsafe {
            let flags = &mut 0;

            let mut whitespace = false;
            let mut newline = false;
            let mut ptr = self.ptr;
            loop {
                ptr = match byte(flags, ptr) {
                    (0, end) if end == self.end => {
                        whitespace = true;
                        newline = true;
                        break;
                    }

                    (b' ' | b'\t' | b'\x0B' | b'\x0C', ptr) => { ptr }
                    (b'\r', ptr) => match *ptr {
                        b'\n' => { newline = true; ptr.offset(1) }
                        _ => { break; }
                    }
                    (b'\n', ptr) => { newline = true; ptr }
                    (b'/', ptr) => match byte(flags, ptr) {
                        (b'/', mut ptr) => loop {
                            ptr = match byte(flags, ptr) {
                                (0, end) if end == self.end => { break ptr; }

                                (b'\n', _) => { break ptr; }
                                (_, ptr) => { ptr }
                            };
                        }
                        (b'*', mut ptr) => loop {
                            ptr = match byte(flags, ptr) {
                                (0, end) if end == self.end => { break ptr; }

                                (b'*', ptr) => match byte(flags, ptr) {
                                    (b'/', ptr) => { break ptr; }
                                    (_, ptr) => { ptr }
                                }
                                (_, ptr) => { ptr }
                            };
                        }
                        _ => { break; }
                    }
                    _ => { break; }
                };
                whitespace = true;
                if horizontal && newline { break; }
            }

            let kind = match (whitespace, newline) {
                (false, _) => { Shape::None }
                (true, true) => { Shape::Newline }
                (true, false) => { Shape::Horizontal }
            };
            let len = ptr.offset_from(self.ptr) as usize;
            self.ptr = ptr;
            Space { kind, len }
        }
    }

    /// Consume a preprocessing token. Whitespace must be skipped first with [`Tokens::whitespace`].
    pub fn preprocessing_token<'i>(
        &mut self, symbols: &'i SymbolMap<Kind>, scratch: &mut Vec<u8>
    ) -> Token<'i, 's> {
        let flags = &mut 0;

        unsafe {
            match byte(flags, self.ptr) {
                (0, end) if end == self.end => { self.make_token(flags, Kind::EndOfFile, self.ptr) }

                (b @ b'L' | b @ b'u' | b @ b'U', ptr) => match byte(flags, ptr) {
                    (b'\'', ptr) => { self.character(flags, ptr) }
                    (b'"', ptr) => { self.string(flags, ptr) }
                    (b'R', ptr) => match byte(flags, ptr) {
                        (b'"', ptr) => { self.raw_string(flags, ptr) }
                        _ => { self.identifier(flags, symbols, scratch, ptr) }
                    }
                    (b'8', ptr) if b == b'u' => match byte(flags, ptr) {
                        (b'\'', ptr) => { self.character(flags, ptr) }
                        (b'"', ptr) => { self.string(flags, ptr) }
                        (b'R', ptr) => match byte(flags, ptr) {
                            (b'"', ptr) => { self.raw_string(flags, ptr) }
                            _ => { self.identifier(flags, symbols, scratch, ptr) }
                        }
                        _ => { self.identifier(flags, symbols, scratch, ptr) }
                    }
                    _ => { self.identifier(flags, symbols, scratch, ptr) }
                }
                (b'R', ptr) => match byte(flags, ptr) {
                    (b'"', ptr) => { self.raw_string(flags, ptr) }
                    _ => { self.identifier(flags, symbols, scratch, ptr) }
                }

                (b'a'..=b'z' | b'A'..=b'Z' | b'_', ptr) => { self.identifier(flags, symbols, scratch, ptr) }
                (b'\\', ptr) => match try_ucn(flags, ptr) {
                    Some((c, ptr)) if is_xid_start(c) => { self.identifier(flags, symbols, scratch, ptr) }
                    _ => { self.make_token(flags, Kind::Error, ptr) }
                }
                (0x80.., ptr) => match try_utf8(self.ptr) {
                    Some((c, ptr)) if is_xid_start(c) => { self.identifier(flags, symbols, scratch, ptr) }
                    _ => { self.make_token(flags, Kind::Error, ptr) }
                }
                (b'0'..=b'9', ptr) => { self.preprocessing_number(flags, ptr) }
                (b'\'', ptr) => { self.character(flags, ptr) }
                (b'"', ptr) => { self.string(flags, ptr) }

                (b'#', ptr) => match byte(flags, ptr) {
                    (b'#', ptr) => { self.make_token(flags, Kind::HashHash, ptr) }
                    _ => { self.make_token(flags, Kind::Hash, ptr) }
                }

                (b'{', ptr) => { self.make_token(flags, Kind::LeftBrace, ptr) }
                (b'}', ptr) => { self.make_token(flags, Kind::RightBrace, ptr) }
                (b'[', ptr) => { self.make_token(flags, Kind::LeftBracket, ptr) }
                (b']', ptr) => { self.make_token(flags, Kind::RightBracket, ptr) }
                (b'(', ptr) => { self.make_token(flags, Kind::LeftParen, ptr) }
                (b')', ptr) => { self.make_token(flags, Kind::RightParen, ptr) }

                (b';', ptr) => { self.make_token(flags, Kind::Semi, ptr) }
                (b':', ptr) => match byte(flags, ptr) {
                    (b'>', ptr) => { self.make_token(flags, Kind::RightBracket, ptr) }
                    (b':', ptr) => { self.make_token(flags, Kind::ColonColon, ptr) }
                    _ => { self.make_token(flags, Kind::Colon, ptr) }
                }
                (b'.', ptr) => match byte(flags, ptr) {
                    (b'0'..=b'9', ptr) => { self.preprocessing_number(flags, ptr) }
                    (b'*', ptr) => { self.make_token(flags, Kind::DotStar, ptr) }
                    (b'.', ptr2) => match byte(flags, ptr2) {
                        (b'.', ptr) => { self.make_token(flags, Kind::Ellipsis, ptr) }
                        _ => { self.make_token(flags, Kind::Dot, ptr) }
                    }
                    _ => { self.make_token(flags, Kind::Dot, ptr) }
                }

                (b'?', ptr) => { self.make_token(flags, Kind::Question, ptr) }
                (b'~', ptr) => { self.make_token(flags, Kind::Tilde, ptr) }

                (b'!', ptr) => match byte(flags, ptr) {
                    (b'=', ptr) => { self.make_token(flags, Kind::ExclaimEq, ptr) }
                    _ => { self.make_token(flags, Kind::Exclaim, ptr) }
                }
                (b'+', ptr) => match byte(flags, ptr) {
                    (b'=', ptr) => { self.make_token(flags, Kind::PlusEq, ptr) }
                    (b'+', ptr) => { self.make_token(flags, Kind::PlusPlus, ptr) }
                    _ => { self.make_token(flags, Kind::Plus, ptr) }
                }
                (b'-', ptr) => match byte(flags, ptr) {
                    (b'=', ptr) => { self.make_token(flags, Kind::MinusEq, ptr) }
                    (b'-', ptr) => { self.make_token(flags, Kind::MinusMinus, ptr) }
                    (b'>', ptr) => match byte(flags, ptr) {
                        (b'*', ptr) => { self.make_token(flags, Kind::ArrowStar, ptr) }
                        _ => { self.make_token(flags, Kind::Arrow, ptr) }
                    }
                    _ => { self.make_token(flags, Kind::Minus, ptr) }
                }
                (b'*', ptr) => match byte(flags, ptr) {
                    (b'=', ptr) => { self.make_token(flags, Kind::StarEq, ptr) }
                    _ => { self.make_token(flags, Kind::Star, ptr) }
                }
                (b'/', ptr) => match byte(flags, ptr) {
                    (b'=', ptr) => { self.make_token(flags, Kind::SlashEq, ptr) }
                    _ => { self.make_token(flags, Kind::Slash, ptr) }
                }
                (b'%', ptr) => match byte(flags, ptr) {
                    (b'>', ptr) => { self.make_token(flags, Kind::RightBrace, ptr) }
                    (b':', ptr) => match byte(flags, ptr) {
                        (b'%', ptr2) => match byte(flags, ptr2) {
                            (b':', ptr2) => { self.make_token(flags, Kind::HashHash, ptr2) }
                            _ => { self.make_token(flags, Kind::Hash, ptr) }
                        }
                        _ => { self.make_token(flags, Kind::Hash, ptr) }
                    }
                    (b'=', ptr) => { self.make_token(flags, Kind::PercentEq, ptr) }
                    _ => { self.make_token(flags, Kind::Percent, ptr) }
                }
                (b'^', ptr) => match byte(flags, ptr) {
                    (b'=', ptr) => { self.make_token(flags, Kind::CaretEq, ptr) }
                    _ => { self.make_token(flags, Kind::Caret, ptr) }
                }
                (b'&', ptr) => match byte(flags, ptr) {
                    (b'&', ptr) => { self.make_token(flags, Kind::AmpAmp, ptr) }
                    (b'=', ptr) => { self.make_token(flags, Kind::AmpEq, ptr) }
                    _ => { self.make_token(flags, Kind::Amp, ptr) }
                }
                (b'|', ptr) => match byte(flags, ptr) {
                    (b'|', ptr) => { self.make_token(flags, Kind::PipePipe, ptr) }
                    (b'=', ptr) => { self.make_token(flags, Kind::PipeEq, ptr) }
                    _ => { self.make_token(flags, Kind::Pipe, ptr) }
                }

                (b'=', ptr) => match byte(flags, ptr) {
                    (b'=', ptr) => { self.make_token(flags, Kind::EqEq, ptr) }
                    _ => { self.make_token(flags, Kind::Eq, ptr) }
                }
                (b'<', ptr) => match byte(flags, ptr) {
                    (b':', end) => match byte(flags, end) {
                        (b':', end2) => match byte(flags, end2) {
                            (b'>' | b':', _) => { self.make_token(flags, Kind::LeftBracket, end) }
                            _ => { self.make_token(flags, Kind::Lt, ptr) }
                        }
                        _ => { self.make_token(flags, Kind::LeftBracket, end) }
                    }
                    (b'%', ptr) => { self.make_token(flags, Kind::LeftBrace, ptr) }
                    (b'=', ptr) => match byte(flags, ptr) {
                        (b'>', ptr) => { self.make_token(flags, Kind::LtEqGt, ptr) }
                        _ => { self.make_token(flags, Kind::LtEq, ptr) }
                    }
                    (b'<', ptr) => match byte(flags, ptr) {
                        (b'=', ptr) => { self.make_token(flags, Kind::LtLtEq, ptr) }
                        _ => { self.make_token(flags, Kind::LtLt, ptr) }
                    }
                    _ => { self.make_token(flags, Kind::Lt, ptr) }
                }
                (b'>', ptr) => match byte(flags, ptr) {
                    (b'=', ptr) => { self.make_token(flags, Kind::GtEq, ptr) }
                    (b'>', ptr) => match byte(flags, ptr) {
                        (b'=', ptr) => { self.make_token(flags, Kind::GtGtEq, ptr) }
                        _ => { self.make_token(flags, Kind::GtGt, ptr) }
                    }
                    _ => { self.make_token(flags, Kind::Gt, ptr) }
                }

                (b',', ptr) => { self.make_token(flags, Kind::Comma, ptr) }

                (_, ptr) => { self.make_token(flags, Kind::Error, ptr) }
            }
        }
    }

    /// Check for and consume a header-name token. Only makes sense in `#include` directives.
    pub fn try_header_name(&mut self) -> Option<Token<'static, 's>> {
        let flags = &mut 0;

        unsafe {
            let (delim, mut ptr) = match byte(flags, self.ptr) {
                (b'"', ptr) => { (b'"', ptr) }
                (b'<', ptr) => { (b'>', ptr) }
                _ => { return None; }
            };

            loop {
                ptr = match byte(flags, ptr) {
                    (0, end) if end == self.end => { return None; }
                    (b'\n', _) => { return None; }
                    (b, end) if b == delim => { ptr = end; break; }
                    (_, ptr) => { ptr }
                };
            }

            let len = ptr.offset_from(self.ptr) as usize;
            let flags = *flags;
            let data = ptr::NonNull::new_unchecked(self.ptr as _);
            let spelling = Spelling::Buffer { flags, data, _marker: PhantomData };
            self.ptr = ptr;
            Some(Token { kind: Kind::HeaderName, spelling, len })
        }
    }

    unsafe fn identifier<'i>(
        &mut self, flags: &mut u8, symbols: &'i SymbolMap<Kind>, scratch: &mut Vec<u8>,
        mut ptr: *const u8
    ) -> Token<'i, 'static> {
        loop {
            ptr = match byte(flags, ptr) {
                (b'0'..=b'9' | b'a'..=b'z' | b'A'..=b'Z' | b'_', ptr) => { ptr }
                (b'\\', ptr) => match try_ucn(flags, ptr) {
                    Some((c, ptr)) if is_xid_continue(c) => { ptr }
                    _ => { break; }
                }
                (0x80.., _) => match try_utf8(ptr) {
                    Some((c, ptr)) if is_xid_continue(c) => { ptr }
                    _ => { break; }
                }
                _ => { break; }
            };
        }

        let len = ptr.offset_from(self.ptr) as usize;
        let ident = if (*flags & FLAG_SPLICE) == 0 && (*flags & FLAG_UCN) == 0 {
            symbols.intern(slice::from_raw_parts(self.ptr, len), Kind::Identifier)
        } else {
            scratch.reserve(len);
            clean_spelling(scratch, Kind::Identifier, self.ptr, ptr);
            let ident = symbols.intern(&scratch[..], Kind::Identifier);
            scratch.clear();
            ident
        };

        let kind = *ident.value();
        self.ptr = ptr;
        Token { kind, len, spelling: Spelling::Intern(ident) }
    }

    unsafe fn preprocessing_number(
        &mut self, flags: &mut u8, mut ptr: *const u8
    ) -> Token<'static, 's> {
        loop {
            ptr = match byte(flags, ptr) {
                (b'e' | b'E' | b'p' | b'P', ptr) => match byte(flags, ptr) {
                    (b'+' | b'-', ptr) => { ptr }
                    _ => { ptr }
                }
                (b'0'..=b'9' | b'a'..=b'z' | b'A'..=b'Z' | b'_' | b'.', ptr) => { ptr }
                (b'\\', ptr) => match try_ucn(flags, ptr) {
                    Some((c, ptr)) if is_xid_continue(c) => { ptr }
                    _ => { break; }
                }
                (0x80.., _) => match try_utf8(ptr) {
                    Some((c, ptr)) if is_xid_continue(c) => { ptr }
                    _ => { break; }
                }
                (b'\'', ptr) => match byte(flags, ptr) {
                    (b'0'..=b'9' | b'a'..=b'z' | b'A'..=b'Z' | b'_', ptr) => { ptr }
                    _ => { break; }
                }
                _ => { break; }
            };
        }

        self.make_token(flags, Kind::Number, ptr)
    }

    unsafe fn character(&mut self, flags: &mut u8, ptr: *const u8) -> Token<'static, 's> {
        match byte(flags, ptr) {
            (b'\'', ptr) => { self.make_token(flags, Kind::Error, ptr) }
            _ => { self.quoted(flags, Kind::Character, b'\'', ptr) }
        }
    }

    unsafe fn string(&mut self, flags: &mut u8, ptr: *const u8) -> Token<'static, 's> {
        self.quoted(flags, Kind::String, b'"', ptr)
    }

    unsafe fn quoted(
        &mut self, flags: &mut u8, token: Kind, delim: u8, mut ptr: *const u8
    ) -> Token<'static, 's> {
        loop {
            ptr = match byte(flags, ptr) {
                (0, end) if end == self.end => { return self.make_token(flags, Kind::Error, ptr); }
                (b'\n', _) => { return self.make_token(flags, Kind::Error, ptr); }
                (b'\\', ptr) => match byte(flags, ptr) {
                    (0, end) if end == self.end => { return self.make_token(flags, Kind::Error, ptr); }
                    (_, ptr) => { ptr }
                }
                (b, end) if b == delim => { ptr = end; break; }
                (_, ptr) => { ptr }
            };
        }

        if let Some(end) = try_ud_suffix(flags, ptr) { ptr = end; }

        self.make_token(flags, token, ptr)
    }

    unsafe fn raw_string(&mut self, flags: &mut u8, mut ptr: *const u8) -> Token<'static, 's> {
        let prefix = ptr;
        let mut len = 0;
        loop {
            ptr = match *ptr {
                b'!' | b'"' | b'#' | b'%' | b'&' | b'\'' | b'*' | b'+' | b',' | b'-' | b'.' | b'/' |
                b'0'..=b'9' | b':' | b';' | b'<' | b'=' | b'>' | b'?' |
                b'A'..=b'Z' | b'[' | b']' | b'^' | b'_' |
                b'a'..=b'z' | b'{' | b'|' | b'}' | b'~' if len < 16 => { len += 1; ptr.offset(1) }
                b'(' => { break; }
                _ => { return self.quoted(flags, Kind::Error, b'"', ptr); }
            }
        }
        ptr = ptr.offset(1);

        loop {
            let b = *ptr;
            let end = ptr.offset(1);
            ptr = match b {
                0 if end == self.end => { return self.make_token(flags, Kind::Error, ptr); }
                b')' if
                    self.end.offset_from(end) > len + 1 &&
                    slice::from_raw_parts(prefix, len as usize) ==
                        slice::from_raw_parts(end, len as usize) &&
                    *end.offset(len) == b'"'
                => { ptr = end.offset(len + 1); break; }
                _ => { end }
            };
        }

        if let Some(end) = try_ud_suffix(flags, ptr) { ptr = end; }

        self.make_token(flags, Kind::String, ptr)
    }

    unsafe fn make_token(
        &mut self, flags: &mut u8, kind: Kind, ptr: *const u8
    ) -> Token<'static, 's> {
        let len = ptr.offset_from(self.ptr) as usize;
        let flags = *flags;
        let data = ptr::NonNull::new_unchecked(self.ptr as _);
        let spelling = Spelling::Buffer { flags, data, _marker: PhantomData };
        self.ptr = ptr;
        Token { kind, len, spelling }
    }
}

unsafe fn try_ud_suffix(flags: &mut u8, mut ptr: *const u8) -> Option<*const u8> {
    ptr = match byte(flags, ptr) {
        (b'a'..=b'z' | b'A'..=b'Z' | b'_', ptr) => { ptr }
        (b'\\', ptr) => match try_ucn(flags, ptr) {
            Some((c, ptr)) if is_xid_start(c) => { ptr }
            _ => { return None; }
        }
        (0x80.., _) => match try_utf8(ptr) {
            Some((c, ptr)) if is_xid_start(c) => { ptr }
            _ => { return None; }
        }
        _ => { return None; }
    };

    loop {
        ptr = match byte(flags, ptr) {
            (b'0'..=b'9' | b'a'..=b'z' | b'A'..=b'Z' | b'_', ptr) => { ptr }
            (b'\\', ptr) => match try_ucn(flags, ptr) {
                Some((c, ptr)) if is_xid_continue(c) => { ptr }
                _ => { break; }
            }
            (0x80.., _) => match try_utf8(ptr) {
                Some((c, ptr)) if is_xid_continue(c) => { ptr }
                _ => { break; }
            }
            _ => { break; }
        };
    }

    Some(ptr)
}

fn is_xid_start(c: char) -> bool {
    ranges_contains(c, &XID_CONTINUE[..]) && !ranges_contains(c, &XID_CONTINUE_NOT_START[..])
}
fn is_xid_continue(c: char) -> bool {
    ranges_contains(c, &XID_CONTINUE[..])
}

fn ranges_contains(c: char, ranges: &[RangeInclusive<u32>]) -> bool {
    let c = c as u32;
    ranges.binary_search_by(move |range| {
        if *range.end() < c {
            Ordering::Less
        } else if c < *range.start() {
            Ordering::Greater
        } else {
            Ordering::Equal
        }
    }).is_ok()
}
static XID_CONTINUE: [RangeInclusive<u32>; 45] = [
    0x00a8..=0x00a8, 0x00aa..=0x00aa, 0x00ad..=0x00ad, 0x00af..=0x00af, 0x00b2..=0x00b5,
    0x00b7..=0x00ba, 0x00bc..=0x00be, 0x00c0..=0x00d6, 0x00d8..=0x00f6, 0x00f8..=0x00ff,

    0x0100..=0x167f, 0x1681..=0x180d, 0x180f..=0x1fff,

    0x200b..=0x200d, 0x202a..=0x202e, 0x203f..=0x2040, 0x2054..=0x2054, 0x2060..=0x206f,

    0x2070..=0x218f, 0x2460..=0x24ff, 0x2776..=0x2793, 0x2c00..=0x2dff, 0x2e80..=0x2fff,

    0x3004..=0x3007, 0x3021..=0x302f, 0x3031..=0x303f,

    0x3040..=0xd7ff,

    0xf900..=0xfd3d, 0xfd40..=0xfdcf, 0xfdf0..=0xfe44, 0xfe47..=0xfffd,

    0x10000..=0x1fffd, 0x20000..=0x2fffd, 0x30000..=0x3fffd, 0x40000..=0x4fffd,
    0x50000..=0x5fffd, 0x60000..=0x6fffd, 0x70000..=0x7fffd, 0x80000..=0x8fffd,
    0x90000..=0x9fffd, 0xa0000..=0xafffd, 0xb0000..=0xbfffd, 0xc0000..=0xcfffd,
    0xd0000..=0xdfffd, 0xe0000..=0xefffd,
];
static XID_CONTINUE_NOT_START: [RangeInclusive<u32>; 4] = [
    0x0300..=0x036f, 0x1dc0..=0x1dff, 0x20d0..=0x20ff, 0xfe20..=0xfe2f,
];

unsafe fn try_ucn(flags: &mut u8, mut ptr: *const u8) -> Option<(char, *const u8)> {
    let len = match *ptr {
        b'u' => { 4 }
        b'U' => { 8 }
        _ => { return None; }
    };
    ptr = ptr.offset(1);

    let mut ch = 0;
    for _ in 0..len {
        let digit = match *ptr {
            b @ b'0'..=b'9' => { b - b'0' }
            b @ b'a'..=b'f' => { b - b'a' }
            b @ b'A'..=b'F' => { b - b'A' }
            _ => { return None; }
        };
        ptr = ptr.offset(1);

        ch = (ch << 4) + digit as u32;
    }

    match ch {
        0x24 | 0x40 | 0x60 => {}
        0x00..=0x9F | 0xD800..=0xDFFF | 0x110000..=u32::MAX => { return None; }
        _ => {}
    }

    *flags |= FLAG_UCN;
    Some((char::from_u32_unchecked(ch), ptr))
}

impl<'i, 's> Token<'i, 's> {
    #[inline]
    pub fn kind(&self) -> Kind { self.kind }

    #[inline]
    pub fn len(&self) -> usize { self.len }

    /// Obtain a clean version of this token's spelling. Opportunistically reuse the source buffer.
    pub fn spelling<'a>(&self, scratch: &'a mut Vec<u8>) -> &'a [u8] where 'i: 'a, 's: 'a {
        let (clean, raw) = self.spelling_raw();
        if clean { return raw; }

        let Range { start: ptr, end } = raw.as_ptr_range();
        let len = scratch.len();
        scratch.reserve(self.len);
        unsafe { clean_spelling(scratch, self.kind, ptr, end); }
        &scratch[len..]
    }

    /// Write a clean version of this token's spelling into `buf`.
    pub fn write_spelling(&self, buf: &mut Vec<u8>) {
        let (clean, raw) = self.spelling_raw();
        if clean { return buf.extend(raw); }

        let Range { start: ptr, end } = raw.as_ptr_range();
        buf.reserve(self.len);
        unsafe { clean_spelling(buf, self.kind, ptr, end) }
    }

    fn spelling_raw<'a>(&self) -> (bool, &'a [u8]) where 'i: 'a, 's: 'a {
        unsafe {
            let (flags, data) = match self.spelling {
                Spelling::Intern(ident) => { return (true, ident.key()); }
                Spelling::Buffer { flags, data, .. } => { (flags, data) }
            };
            let clean = (flags & FLAG_SPLICE) == 0 && (flags & FLAG_UCN) == 0;
            let raw = slice::from_raw_parts(data.as_ptr() as *const _, self.len);
            (clean, raw)
        }
    }
}

/// Write the spelling of the token in `ptr..end` to `buf`, without splices or UCNs.
///
/// # Safety
///
/// `ptr` must point into a `[u8]` which ends with `b"\0\0\0"`, before that suffix.
/// (If `ptr` points to a valid UTF-8 code point, no suffix is required.)
/// String and character tokens must be correctly delimited.
#[cold]
unsafe fn clean_spelling(buf: &mut Vec<u8>, kind: Kind, mut ptr: *const u8, end: *const u8) {
    let flags = &mut 0;

    let mut ucn = ptr;
    if kind == Kind::HeaderName {
        // No UCNs in header-names.
        ucn = end;
    } else if kind == Kind::String {
        // No UCNs in strings before the ud-suffix.
        for i in 1.. { ucn = end.offset(-i); if *ucn == b'"' { break; }  }

        // No splice removal between raw string quotes (but splices must be removed from
        // the prefix to correctly detect raw strings).
        let len = buf.len();
        while ptr != end {
            let (b, end) = byte(flags, ptr);
            buf.push(b);
            ptr = end;
            if b == b'"' { break; }
        }
        if let [.., b'R', b'"'] = buf[len..] {
            buf.extend(slice::from_raw_parts(ptr, ucn.offset_from(ptr) as usize));
            ptr = ucn;
        }
    } else if kind == Kind::Character {
        // No UCNs in characters before the ud-suffix.
        for i in 1.. { ucn = end.offset(-i); if *ucn == b'\'' { break; } }
    }

    while ptr != end {
        ptr = match byte(flags, ptr) {
            (b @ b'\\', ptr) if ucn < ptr => match try_ucn(flags, ptr) {
                Some((c, ptr)) => {
                    let mut str = [0; 4];
                    let str = c.encode_utf8(&mut str[..]);
                    buf.extend(str.as_bytes());
                    ptr
                }
                None => { buf.push(b); ptr }
            }
            (b, ptr) => { buf.push(b); ptr }
        };
    }
}

/// Translation phase 2 - physical source lines spliced to logical source lines
///
/// Safety: `ptr` must point into a `[u8]` which ends with a `0`.
#[inline]
unsafe fn byte(flags: &mut u8, ptr: *const u8) -> (u8, *const u8) {
    let b = *ptr;
    if b == b'\\' { return byte_spliced(flags, ptr); }
    (b, ptr.offset(1))
}

// TODO: Consider copying one full logical line at a time into a separate buffer.
// Performance-wise, this might help by moving the branch on `b'\\'` out of other hot paths,
// potentially along with UCN replacement (need to investigate UCN/spelling interaction),
// though this would interfere with raw strings and macros by restricting token lifetimes.
// API-wise it might help isolate the buffer safety requirements from lexer clients.
#[cold]
unsafe fn byte_spliced(flags: &mut u8, ptr: *const u8) -> (u8, *const u8) {
    let mut end = ptr.offset(1);
    while let b'\t' | b'\x0B' | b'\x0C' | b' ' = *end { end = end.offset(1); }
    if *end == b'\r' && *end.offset(1) == b'\n' { end = end.offset(1); }
    match *end {
        b'\n' => { *flags |= FLAG_SPLICE; byte(flags, end.offset(1)) }
        _ => { (b'\\', ptr.offset(1)) }
    }
}

/// Translation phase 1 - physical source file characters mapped to translation character set
///
/// # Safety
///
/// `ptr` must point into a `[u8]` which ends with `b"\0\0\0"`, before that suffix.
/// (If `ptr` points to a valid UTF-8 code point, no suffix is required.)
unsafe fn try_utf8(ptr: *const u8) -> Option<(char, *const u8)> {
    let b = *ptr;
    let (len, mask, min) = match b {
        0b0000_0000..=0b0111_1111 => { (1, 0b0111_1111, 0) }
        0b1000_0000..=0b1011_1111 => { return None; }
        0b1100_0000..=0b1101_1111 => { (2, 0b0001_1111, 0b1000_0000) }
        0b1110_0000..=0b1110_1111 => { (3, 0b0000_1111, 0b1000_0000_0000) }
        0b1111_0000..=0b1111_0111 => { (4, 0b0000_0111, 0b1_0000_0000_0000_0000) }
        0b1111_1000..=0b1111_1111 => { return None; }
    };

    let mut ch = (b & mask) as u32;
    for i in 1..len {
        match *ptr.offset(i) {
            0b1000_0000..=0b1011_1111 => { ch = (ch << 6) + (b & 0b0011_1111) as u32; }
            _ => { return None; }
        }
    }
    if ch < min { return None; }

    Some((char::from_u32_unchecked(ch), ptr.offset(len)))
}

#[cfg(test)]
mod tests {
    use super::{Tokens, Shape, Kind};
    use crate::symbols::SymbolMap;

    #[test]
    fn hello() {
        let symbols = &mut SymbolMap::default();
        Tokens::alternative_tokens(symbols);

        let mut tokens = Tokens::from_bytes_with_padding(
b"if (x < 3) {
    printf(\"hello world\");
}
\0\0\0"
        ).unwrap();
        let scratch = &mut Vec::default();

        let result = [
            (Shape::None, 0, Kind::Identifier),
            (Shape::Horizontal, 1, Kind::LeftParen),
            (Shape::None, 0, Kind::Identifier),
            (Shape::Horizontal, 1, Kind::Lt),
            (Shape::Horizontal, 1, Kind::Number),
            (Shape::None, 0, Kind::RightParen),
            (Shape::Horizontal, 1, Kind::LeftBrace),
            (Shape::Newline, 5, Kind::Identifier),
            (Shape::None, 0, Kind::LeftParen),
            (Shape::None, 0, Kind::String),
            (Shape::None, 0, Kind::RightParen),
            (Shape::None, 0, Kind::Semi),
            (Shape::Newline, 1, Kind::RightBrace),
            (Shape::Newline, 1, Kind::EndOfFile),
        ];
        for (shape, len, kind) in result {
            let space = tokens.whitespace(false);
            assert!((space.kind, space.len) == (shape, len));
            let token = tokens.preprocessing_token(symbols, scratch);
            assert_eq!(token.kind(), kind);
        }
    }

    // TODO: test identifiers starting with `R`, `u`, etc
}
