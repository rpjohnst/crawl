use std::mem;
use std::cell::Cell;
use std::collections::HashMap;

use crate::lex::{self, Shape};
use crate::symbols::{SymbolMap, Symbol};

/// Translation phase 4 - preprocessing directives and macro expansion
pub struct Preprocessor<'i, 's> {
    source: &'s dyn Source,

    conditionals: Vec<Conditional>,
    lexers: Vec<(usize, lex::Tokens<'s>)>,
    macros: HashMap<Symbol<'i, lex::Kind>, Macro<'i, 's>>,

    if_: Symbol<'i, lex::Kind>,
    ifdef: Symbol<'i, lex::Kind>,
    ifndef: Symbol<'i, lex::Kind>,
    elif: Symbol<'i, lex::Kind>,
    else_: Symbol<'i, lex::Kind>,
    endif: Symbol<'i, lex::Kind>,

    defined: Symbol<'i, lex::Kind>,
    #[allow(unused)] has_include: Symbol<'i, lex::Kind>,
    #[allow(unused)] has_cpp_attribute: Symbol<'i, lex::Kind>,
    true_: Symbol<'i, lex::Kind>,
    false_: Symbol<'i, lex::Kind>,

    include: Symbol<'i, lex::Kind>,
    define: Symbol<'i, lex::Kind>,
    undef: Symbol<'i, lex::Kind>,
    line: Symbol<'i, lex::Kind>,
    error: Symbol<'i, lex::Kind>,
    pragma: Symbol<'i, lex::Kind>,

    va_args: Symbol<'i, lex::Kind>,
    va_opt: Symbol<'i, lex::Kind>,
}

pub trait Source {
    fn lexer_for_header(&self, angle: bool, header: &[u8]) -> Option<lex::Tokens<'_>>;
    fn lexer_for_buffer(&self, buffer: &[u8]) -> lex::Tokens<'_>;
}

struct Conditional { live: bool, taken: bool, has_else: bool }

struct Value { unsigned: bool, value: u64 }

struct Macro<'i, 's> {
    parameters: Option<Vec<Parameter<'i>>>,
    replacement: Vec<Token<'i, 's>>,
    active: Cell<bool>,
}

struct Parameter<'i> { name: Symbol<'i, lex::Kind>, replaced: bool, stringized: bool }

#[derive(Copy, Clone)]
struct Token<'i, 's> { space: lex::Space, token: lex::Token<'i, 's>, replace: bool }

pub struct Tokens<'i, 's> {
    newline: bool,
    offset: usize,

    macros: Vec<Invoked<'i>>,
    buffer: Vec<Token<'i, 's>>,

    conditionals: usize,
    tokens: lex::Tokens<'s>,
    scratch: Vec<u8>,
}

struct Invoked<'i> { name: Symbol<'i, lex::Kind>, begin: usize }

impl<'i, 's> Preprocessor<'i, 's> {
    pub fn new(symbols: &'i SymbolMap<lex::Kind>, source: &'s dyn Source) -> Preprocessor<'i, 's> {
        let conditionals = Vec::default();
        let lexers = Vec::default();
        let macros = HashMap::default();

        let if_ = symbols.intern(b"if", lex::Kind::Identifier);
        let ifdef = symbols.intern(b"ifdef", lex::Kind::Identifier);
        let ifndef = symbols.intern(b"ifndef", lex::Kind::Identifier);
        let elif = symbols.intern(b"elif", lex::Kind::Identifier);
        let else_ = symbols.intern(b"else", lex::Kind::Identifier);
        let endif = symbols.intern(b"endif", lex::Kind::Identifier);

        let defined = symbols.intern(b"defined", lex::Kind::Identifier);
        let has_include = symbols.intern(b"__has_include", lex::Kind::Identifier);
        let has_cpp_attribute = symbols.intern(b"__has_cpp_attribute", lex::Kind::Identifier);
        let true_ = symbols.intern(b"true", lex::Kind::Identifier);
        let false_ = symbols.intern(b"false", lex::Kind::Identifier);

        let include = symbols.intern(b"include", lex::Kind::Identifier);
        let define = symbols.intern(b"define", lex::Kind::Identifier);
        let undef = symbols.intern(b"undef", lex::Kind::Identifier);
        let line = symbols.intern(b"line", lex::Kind::Identifier);
        let error = symbols.intern(b"error", lex::Kind::Identifier);
        let pragma = symbols.intern(b"pragma", lex::Kind::Identifier);

        let va_args = symbols.intern(b"__VA_ARGS__", lex::Kind::Identifier);
        let va_opt = symbols.intern(b"__VA_OPT__", lex::Kind::Identifier);

        Preprocessor {
            source,
            conditionals, lexers, macros,
            if_, ifdef, ifndef, elif, else_, endif,
            defined, has_include, has_cpp_attribute, true_, false_,
            include, define, undef, line, error, pragma,
            va_args, va_opt,
        }
    }

    pub fn tokens(&self, tokens: lex::Tokens<'s>) -> Tokens<'i, 's> {
        let macros = Vec::default();
        let buffer = Vec::default();

        let conditionals = self.conditionals.len();
        let scratch = Vec::default();

        Tokens { newline: true, offset: 0, macros, buffer, conditionals, tokens, scratch }
    }
}

impl<'i, 's> Tokens<'i, 's> {
    pub fn preprocessed_token(
        &mut self, cpp: &mut Preprocessor<'i, 's>, symbols: &'i SymbolMap<lex::Kind>
    ) -> lex::Token<'i, 's> {
        loop {
            let token = self.expanded_token(cpp, symbols, false);
            match token.token.kind() {
                lex::Kind::EndOfFile => if let Some((conditionals, tokens)) = cpp.lexers.pop() {
                    let conditionals = mem::replace(&mut self.conditionals, conditionals);
                    let _ = mem::replace(&mut self.tokens, tokens);
                    cpp.conditionals.truncate(conditionals);
                    continue;
                }
                lex::Kind::Hash => if token.space.kind == lex::Shape::Newline {
                    self.directive(cpp, symbols);
                    continue;
                }
                _ => {}
            }
            break token.token;
        }
    }

    fn directive(&mut self, cpp: &mut Preprocessor<'i, 's>, symbols: &'i SymbolMap<lex::Kind>) {
        assert!(self.buffer.is_empty());

        let Token { token, .. } = self.unexpanded_token(cpp, symbols, true);
        let ident = match token.kind() {
            lex::Kind::EndOfLine => { return; }
            lex::Kind::Identifier => { token.ident() }
            _ => { return self.discard_directive(cpp, symbols); }
        };

        if ident == cpp.if_ {
            return self.if_directive(cpp, symbols);
        } else if ident == cpp.ifdef {
            return self.ifdef_directive(cpp, symbols, true);
        } else if ident == cpp.ifndef {
            return self.ifdef_directive(cpp, symbols, false);
        } else if ident == cpp.elif {
            return self.elif_directive(cpp, symbols);
        } else if ident == cpp.else_ {
            return self.else_directive(cpp, symbols);
        } else if ident == cpp.endif {
            return self.endif_directive(cpp, symbols);
        } else if ident == cpp.include {
            return self.include_directive(cpp, symbols);
        } else if ident == cpp.define {
            return self.define_directive(cpp, symbols);
        } else if ident == cpp.undef {
            return self.undef_directive(cpp, symbols);
        } else if ident == cpp.line {
            return self.line_directive(cpp, symbols);
        } else if ident == cpp.error {
            return self.error_directive(cpp, symbols);
        } else if ident == cpp.pragma {
            return self.pragma_directive(cpp, symbols);
        } else {
            return self.discard_directive(cpp, symbols);
        }
    }

    fn if_directive(&mut self, cpp: &mut Preprocessor<'i, 's>, symbols: &'i SymbolMap<lex::Kind>) {
        let mut token = self.expanded_token(cpp, symbols, true);
        let active = match self.constant_expression(cpp, symbols, &mut token, true, 3) {
            Some(value) => { value.value != 0 }
            None => { false }
        };
        match token.token.kind() {
            lex::Kind::EndOfLine => {}
            _ => { self.discard_directive(cpp, symbols); }
        }

        cpp.conditionals.push(Conditional { live: true, taken: active, has_else: false });
        if active { return; }

        self.skip_groups(cpp, symbols);
    }

    fn ifdef_directive(
        &mut self, cpp: &mut Preprocessor<'i, 's>, symbols: &'i SymbolMap<lex::Kind>, positive: bool
    ) {
        let Token { token, .. } = self.unexpanded_token(cpp, symbols, true);
        let mut active = match token.kind() {
            lex::Kind::EndOfLine => { false }
            lex::Kind::Identifier => { cpp.macros.contains_key(&token.ident()) }
            _ => { self.discard_directive(cpp, symbols); false }
        };
        let Token { token, .. } = self.unexpanded_token(cpp, symbols, true);
        match token.kind() {
            lex::Kind::EndOfLine => {}
            _ => { self.discard_directive(cpp, symbols); }
        }

        if !positive { active = !active; }
        cpp.conditionals.push(Conditional { live: true, taken: active, has_else: false });
        if active { return; }

        self.skip_groups(cpp, symbols);
    }

    fn elif_directive(
        &mut self, cpp: &mut Preprocessor<'i, 's>, symbols: &'i SymbolMap<lex::Kind>
    ) {
        self.discard_directive(cpp, symbols);

        if self.conditionals == cpp.conditionals.len() {
            return;
        }

        self.skip_groups(cpp, symbols);
    }

    fn else_directive(
        &mut self, cpp: &mut Preprocessor<'i, 's>, symbols: &'i SymbolMap<lex::Kind>
    ) {
        let Token { token, .. } = self.unexpanded_token(cpp, symbols, true);
        match token.kind() {
            lex::Kind::EndOfLine => {}
            _ => { self.discard_directive(cpp, symbols); }
        }

        if self.conditionals == cpp.conditionals.len() {
            return;
        }
        let conditional = cpp.conditionals.last_mut().unwrap();
        conditional.has_else = true;

        self.skip_groups(cpp, symbols);
    }

    fn endif_directive(
        &mut self, cpp: &mut Preprocessor<'i, 's>, symbols: &'i SymbolMap<lex::Kind>
    ) {
        let Token { token, .. } = self.unexpanded_token(cpp, symbols, true);
        match token.kind() {
            lex::Kind::EndOfLine => {}
            _ => { self.discard_directive(cpp, symbols); }
        }

        if self.conditionals == cpp.conditionals.len() {
            return;
        }
    }

    fn skip_groups(&mut self, cpp: &mut Preprocessor<'i, 's>, symbols: &'i SymbolMap<lex::Kind>) {
        loop {
            let token = self.unexpanded_token(cpp, symbols, false);
            match token.token.kind() {
                lex::Kind::EndOfFile => { return; }
                lex::Kind::Hash if token.space.kind == lex::Shape::Newline => {}
                _ => { continue; }
            }

            let Token { token, .. } = self.unexpanded_token(cpp, symbols, true);
            let ident = match token.kind() {
                lex::Kind::EndOfLine => { continue; }
                lex::Kind::Identifier => { token.ident() }
                _ => { self.discard_directive(cpp, symbols); continue; }
            };

            if ident == cpp.if_ || ident == cpp.ifdef || ident == cpp.ifndef {
                self.discard_directive(cpp, symbols);
                cpp.conditionals.push(Conditional { live: false, taken: false, has_else: false });
                continue;
            }

            if ident == cpp.elif {
                let conditional = cpp.conditionals.last().unwrap();
                if !conditional.live || conditional.taken {
                    self.discard_directive(cpp, symbols);
                    continue;
                }

                let mut token = self.expanded_token(cpp, symbols, true);
                let value = match self.constant_expression(cpp, symbols, &mut token, true, 3) {
                    Some(value) => { value }
                    None => { continue; }
                };
                match token.token.kind() {
                    lex::Kind::EndOfLine => {}
                    _ => { self.discard_directive(cpp, symbols); continue; }
                }

                if value.value != 0 {
                    cpp.conditionals.last_mut().unwrap().taken = true;
                    return;
                }
            } else if ident == cpp.else_ {
                let conditional = cpp.conditionals.last_mut().unwrap();
                conditional.has_else = true;
                if !conditional.live || conditional.taken {
                    self.discard_directive(cpp, symbols);
                    continue;
                }

                let token = self.unexpanded_token(cpp, symbols, true);
                match token.token.kind() {
                    lex::Kind::EndOfLine => {}
                    _ => { self.discard_directive(cpp, symbols); continue; }
                }

                cpp.conditionals.last_mut().unwrap().taken = true;
                return;
            } else if ident == cpp.endif {
                let conditional = cpp.conditionals.pop().unwrap();
                if !conditional.live {
                    self.discard_directive(cpp, symbols);
                    continue;
                }

                let token = self.unexpanded_token(cpp, symbols, true);
                match token.token.kind() {
                    lex::Kind::EndOfLine => {}
                    _ => { self.discard_directive(cpp, symbols); }
                }

                return;
            }
        }
    }

    fn constant_expression(
        &mut self, cpp: &Preprocessor<'i, 's>, symbols: &'i SymbolMap<lex::Kind>,
        token: &mut Token<'i, 's>, live: bool, precedence: u8
    ) -> Option<Value> {
        let mut left = match token.token.kind() {
            lex::Kind::EndOfLine => { return None; }
            lex::Kind::Identifier => {
                let ident = token.token.ident();
                if ident == cpp.defined {
                    if !self.macros.is_empty() {
                    }

                    *token = self.unexpanded_token(cpp, symbols, true);
                    let mut paren = false;
                    match token.token.kind() {
                        lex::Kind::EndOfLine => { return None; }
                        lex::Kind::LeftParen => {
                            paren = true;
                            *token = self.unexpanded_token(cpp, symbols, true);
                        }
                        _ => {}
                    }

                    let name = match token.token.kind() {
                        lex::Kind::EndOfLine => { return None; }
                        lex::Kind::Identifier => { token.token.ident() }
                        _ => {
                            self.discard_directive(cpp, symbols);
                            return None;
                        }
                    };

                    let unsigned = false;
                    let value = cpp.macros.contains_key(&name) as u64;

                    if paren {
                        *token = self.unexpanded_token(cpp, symbols, true);
                        match token.token.kind() {
                            lex::Kind::EndOfLine => { return None; }
                            lex::Kind::RightParen => {}
                            _ => {
                                self.discard_directive(cpp, symbols);
                                return None;
                            }
                        }
                    }

                    *token = self.expanded_token(cpp, symbols, true);
                    Value { unsigned, value }
                } else if ident == cpp.true_ {
                    *token = self.expanded_token(cpp, symbols, true);
                    Value { unsigned: false, value: 1 }
                } else if ident == cpp.false_ {
                    *token = self.expanded_token(cpp, symbols, true);
                    Value { unsigned: false, value: 0 }
                } else {
                    *token = self.expanded_token(cpp, symbols, true);
                    Value { unsigned: false, value: 0 }
                }
            }
            lex::Kind::Number => {
                let number = match token.token.number(symbols, &mut self.scratch) {
                    Some(number) => { number }
                    None => {
                        self.discard_directive(cpp, symbols);
                        return None;
                    }
                };
                if number.float() {
                    self.discard_directive(cpp, symbols);
                    return None;
                }
                if let Some(lex::Size::User(_)) = number.size() {
                }

                let mut unsigned = number.unsigned();
                let max = if unsigned { u64::MAX } else { i64::MAX as u64 };
                let (overflow, value) = number.integer_value(max);
                if overflow {
                    // error
                    unsigned = true;
                }
                self.scratch.clear();

                *token = self.expanded_token(cpp, symbols, true);
                Value { unsigned, value }
            }
            lex::Kind::Character => {
                let character = match token.token.character(symbols, &mut self.scratch) {
                    Some(character) => { character }
                    None => {
                        self.discard_directive(cpp, symbols);
                        return None;
                    }
                };
                if let Some(_) = character.suffix() {
                }

                let unsigned = character.encoding().is_some();
                let value = character.value();

                *token = self.expanded_token(cpp, symbols, true);
                Value { unsigned, value }
            }
            lex::Kind::LeftParen => {
                let value = self.constant_expression(cpp, symbols, token, live, 1)?;

                match token.token.kind() {
                    lex::Kind::EndOfLine => { return None; }
                    lex::Kind::RightParen => {}
                    _ => {
                        self.discard_directive(cpp, symbols);
                        return None;
                    }
                }

                *token = self.expanded_token(cpp, symbols, true);
                value
            }
            lex::Kind::Plus => {
                let value = self.constant_expression(cpp, symbols, token, live, 15)?;
                value
            }
            lex::Kind::Minus => {
                let mut value = self.constant_expression(cpp, symbols, token, live, 15)?;
                value.value = -(value.value as i64) as u64;
                value
            }
            lex::Kind::Tilde => {
                let mut value = self.constant_expression(cpp, symbols, token, live, 15)?;
                value.value = !value.value;
                value
            }
            lex::Kind::Exclaim => {
                let mut value = self.constant_expression(cpp, symbols, token, live, 15)?;
                value.unsigned = false;
                value.value = !(value.value != 0) as u64;
                value
            }
            _ => {
                self.discard_directive(cpp, symbols);
                return None;
            }
        };

        loop {
            let operator = token.token.kind();
            let op = match operator {
                lex::Kind::Star | lex::Kind::Slash | lex::Kind::Percent => { 14 }
                lex::Kind::Plus | lex::Kind::Minus => { 13 }
                lex::Kind::LtLt | lex::Kind::GtGt => { 12 }
                lex::Kind::Lt | lex::Kind::Gt | lex::Kind::LtEq | lex::Kind::GtEq => { 10 }
                lex::Kind::EqEq | lex::Kind::ExclaimEq => { 9 }
                lex::Kind::Amp => { 8 }
                lex::Kind::Caret => { 7 }
                lex::Kind::Pipe => { 6 }
                lex::Kind::AmpAmp => { 5 }
                lex::Kind::PipePipe => { 4 }
                lex::Kind::Question => { 3 }
                lex::Kind::Comma => { 1 }
                lex::Kind::Colon | lex::Kind::RightParen | lex::Kind::EndOfLine => { 0 }
                _ => {
                    self.discard_directive(cpp, symbols);
                    return None;
                }
            };
            if op < precedence { return Some(left); }

            *token = self.expanded_token(cpp, symbols, true);
            let right_live = match operator {
                lex::Kind::AmpAmp if left.value == 0 => { false }
                lex::Kind::PipePipe if left.value != 0 => { false }
                lex::Kind::Question if left.value == 0 => { false }
                _ => { live }
            };
            let right_precedence = match operator {
                lex::Kind::Question => { 1 }
                _ => { op + 1 }
            };
            let mut right = self.constant_expression(cpp, symbols, token, right_live, right_precedence)?;

            let mut unsigned = match operator {
                lex::Kind::LtLt | lex::Kind::GtGt => { left.unsigned }
                lex::Kind::AmpAmp | lex::Kind::PipePipe |
                lex::Kind::Question |
                lex::Kind::Comma => { false }
                _ => {
                    let unsigned = left.unsigned | right.unsigned;
                    left.unsigned = unsigned;
                    right.unsigned = unsigned;
                    match operator {
                        lex::Kind::Lt | lex::Kind::Gt | lex::Kind::LtEq | lex::Kind::GtEq |
                        lex::Kind::EqEq | lex::Kind::ExclaimEq |
                        lex::Kind::AmpAmp | lex::Kind::PipePipe => { false }
                        _ => { unsigned }
                    }
                }
            };
            let value = match operator {
                lex::Kind::Star => { u64::wrapping_mul(left.value, right.value) }
                lex::Kind::Slash => if right.value != 0 {
                    if unsigned {
                        u64::wrapping_div(left.value, right.value)
                    } else {
                        i64::wrapping_div(left.value as i64, right.value as i64) as u64
                    }
                } else if live {
                    if token.token.kind() != lex::Kind::EndOfLine {
                        self.discard_directive(cpp, symbols);
                    }
                    return None;
                } else {
                    0
                }
                lex::Kind::Percent => if right.value != 0 {
                    if unsigned {
                        u64::wrapping_rem(left.value, right.value)
                    } else {
                        i64::wrapping_rem(left.value as i64, right.value as i64) as u64
                    }
                } else if live {
                    if token.token.kind() != lex::Kind::EndOfLine {
                        self.discard_directive(cpp, symbols);
                    }
                    return None;
                } else {
                    0
                }
                lex::Kind::Plus => { u64::wrapping_add(left.value, right.value) }
                lex::Kind::Minus => { u64::wrapping_sub(left.value, right.value) }
                lex::Kind::LtLt => if right.value >= 64 { 0 } else { left.value << right.value }
                lex::Kind::GtGt => {
                    let right = if right.value < 64 { right.value } else { 63 };
                    if unsigned {
                        left.value >> right
                    } else {
                        (left.value as i64 >> right) as u64
                    }
                }
                lex::Kind::Lt => if unsigned {
                    (left.value < right.value) as u64
                } else {
                    ((left.value as i64) < (right.value as i64)) as u64
                }
                lex::Kind::Gt => if unsigned {
                    (left.value > right.value) as u64
                } else {
                    ((left.value as i64) > (right.value as i64)) as u64
                }
                lex::Kind::LtEq => if unsigned {
                    (left.value <= right.value) as u64
                } else {
                    ((left.value as i64) <= (right.value as i64)) as u64
                }
                lex::Kind::GtEq => if unsigned {
                    (left.value >= right.value) as u64
                } else {
                    ((left.value as i64) >= (right.value as i64)) as u64
                }
                lex::Kind::EqEq => { (left.value == right.value) as u64 }
                lex::Kind::ExclaimEq => { (left.value != right.value) as u64 }
                lex::Kind::Amp => { left.value & right.value }
                lex::Kind::Caret => { left.value ^ right.value }
                lex::Kind::Pipe => { left.value | right.value }
                lex::Kind::AmpAmp => { (left.value != 0 && right.value != 0) as u64 }
                lex::Kind::PipePipe => { (left.value != 0 || right.value != 0) as u64 }
                lex::Kind::Question => {
                    match token.token.kind() {
                        lex::Kind::EndOfLine => { return None; }
                        lex::Kind::Colon => {}
                        _ => {
                            self.discard_directive(cpp, symbols);
                            return None;
                        }
                    }

                    *token = self.expanded_token(cpp, symbols, true);
                    let else_live = left.value != 0;
                    let else_precedence = op;
                    let else_ = self.constant_expression(cpp, symbols, token, else_live, else_precedence)?;

                    unsigned = right.unsigned | else_.unsigned;
                    if left.value != 0 { right.value } else { else_.value }
                }
                lex::Kind::Comma => {
                    unsigned = right.unsigned;
                    right.value
                }
                _ => { unreachable!() }
            };
            left = Value { unsigned, value };
        }
    }

    fn include_directive(
        &mut self, cpp: &mut Preprocessor<'i, 's>, symbols: &'i SymbolMap<lex::Kind>
    ) {
        let space = self.tokens.whitespace(false);
        if space.kind == lex::Shape::Newline { self.newline = true; }
        self.offset += space.len;

        if self.newline { return; }

        let mut scratch = Vec::default();
        let name = if let Some(token) = self.tokens.try_header_name() {
            self.newline = false;
            self.offset += token.len();

            token.spelling(&mut scratch)
        } else {
            let token = self.expanded_token(cpp, symbols, true);
            match token.token.kind() {
                lex::Kind::EndOfLine => { return; }
                lex::Kind::Lt => {
                    token.token.write_spelling(&mut scratch);
                    loop {
                        let token = self.expanded_token(cpp, symbols, true);
                        if token.token.kind() == lex::Kind::EndOfLine { return; }

                        if token.space.kind != lex::Shape::None { scratch.push(b' '); }
                        token.token.write_spelling(&mut scratch);

                        if token.token.kind() == lex::Kind::Gt { break; }
                    }
                }
                lex::Kind::String => { token.token.write_spelling(&mut scratch); }
                _ => { return self.discard_directive(cpp, symbols); }
            }

            &scratch[..]
        };
        let (angle, header) = match name[..] {
            [b'<', ref header @ .., b'>'] if !header.is_empty() => { (true, header) }
            [b'"', ref header @ .., b'"'] if !header.is_empty() => { (false, header) }
            _ => { return; }
        };

        let Token { token, .. } = self.unexpanded_token(cpp, symbols, true);
        match token.kind() {
            lex::Kind::EndOfLine => {}
            _ => { return self.discard_directive(cpp, symbols); }
        }

        let tokens = match cpp.source.lexer_for_header(angle, header) {
            Some(tokens) => { tokens }
            None => { return; }
        };

        let conditionals = mem::replace(&mut self.conditionals, cpp.conditionals.len());
        let tokens = mem::replace(&mut self.tokens, tokens);
        cpp.lexers.push((conditionals, tokens));
    }

    fn define_directive(
        &mut self, cpp: &mut Preprocessor<'i, 's>, symbols: &'i SymbolMap<lex::Kind>
    ) {
        let Token { token, .. } = self.unexpanded_token(cpp, symbols, true);
        let name = match token.kind() {
            lex::Kind::EndOfLine => { return; }
            lex::Kind::Identifier => { token.ident() }
            _ => { return self.discard_directive(cpp, symbols); }
        };

        let mut variable = false;
        let mut parameters = if let Some(len) = self.tokens.try_lparen() {
            self.newline = false;
            self.offset += len;

            let mut parameters = Vec::default();
            loop {
                let Token { token, .. } = self.unexpanded_token(cpp, symbols, true);
                match token.kind() {
                    lex::Kind::EndOfLine => { return; }
                    lex::Kind::RightParen if parameters.len() == 0 => { break; }
                    lex::Kind::Identifier => {
                        let name = token.ident();
                        parameters.push(Parameter { name, replaced: false, stringized: false });
                    }
                    lex::Kind::Ellipsis => {
                        let name = cpp.va_args;
                        parameters.push(Parameter { name, replaced: false, stringized: false });
                        variable = true;
                    }
                    _ => { return self.discard_directive(cpp, symbols); }
                }

                let Token { token, .. } = self.unexpanded_token(cpp, symbols, true);
                match token.kind() {
                    lex::Kind::EndOfLine => { return; }
                    lex::Kind::Comma if !variable => { continue; }
                    lex::Kind::RightParen => { break; }
                    _ => { return self.discard_directive(cpp, symbols); }
                }
            }

            Some(parameters)
        } else {
            None
        };

        let mut replacement = Vec::default();
        let mut va_opt_cx = None;
        loop {
            let mut token = self.unexpanded_token(cpp, symbols, true);
            if token.token.kind() == lex::Kind::EndOfLine { break; }

            if token.token.kind() == lex::Kind::Identifier {
                let name = token.token.ident();
                if !variable && (name == cpp.va_args || name == cpp.va_opt) {
                    return self.discard_directive(cpp, symbols);
                }

                if name == cpp.va_opt {
                    if va_opt_cx.is_some() {
                        return self.discard_directive(cpp, symbols);
                    }

                    replacement.push(token);
                    va_opt_cx = Some((replacement.len() + 1, 0));

                    token = self.unexpanded_token(cpp, symbols, true);
                    match token.token.kind() {
                        lex::Kind::EndOfLine => { break; }
                        lex::Kind::LeftParen => {}
                        _ => { return self.discard_directive(cpp, symbols); }
                    }
                }
            }

            if let Some((begin, ref mut parens)) = va_opt_cx {
                match token.token.kind() {
                    lex::Kind::LeftParen => { *parens += 1; }
                    lex::Kind::RightParen => { *parens -= 1; }
                    _ => {}
                }

                if *parens == 0 {
                    match replacement[begin..] {
                        [Token { ref token, .. }, ..] if token.kind() == lex::Kind::HashHash => {
                            return self.discard_directive(cpp, symbols);
                        }
                        [.., Token { ref token, .. }] if token.kind() == lex::Kind::HashHash => {
                            return self.discard_directive(cpp, symbols);
                        }
                        _ => {}
                    }

                    let parameters = parameters.as_deref_mut().unwrap();
                    let parameter = parameters.last_mut().unwrap();
                    assert_eq!(parameter.name, cpp.va_args);
                    parameter.replaced = true;

                    va_opt_cx = None;
                }
            }

            replacement.push(token);
        }
        if va_opt_cx.is_some() {
            return;
        }
        match replacement[..] {
            [Token { ref token, .. }, ..] if token.kind() == lex::Kind::HashHash => { return; }
            [.., Token { ref token, .. }] if token.kind() == lex::Kind::HashHash => { return; }
            _ => {}
        }

        if let Some(ref mut parameters) = parameters {
            for i in 0..replacement.len() {
                let Token { ref token, .. } = replacement[i];
                let prev = i.checked_sub(1).and_then(|i| replacement.get(i))
                    .map_or(lex::Kind::Error, |&Token { ref token, .. }| token.kind());
                let next = replacement.get(i + 1)
                    .map_or(lex::Kind::Error, |&Token { ref token, .. }| token.kind());

                if token.kind() == lex::Kind::Identifier {
                    let name = token.ident();
                    let parameter = parameters.iter_mut()
                        .find(move |parameter| parameter.name == name);
                    if let Some(parameter) = parameter {
                        if prev == lex::Kind::Hash {
                            parameter.stringized = true;
                        } else if prev != lex::Kind::HashHash && next != lex::Kind::HashHash {
                            parameter.replaced = true;
                        }
                        continue;
                    } else if name == cpp.va_opt {
                        continue;
                    }
                }

                if prev == lex::Kind::Hash {
                    return;
                }
            }
        }

        let active = Cell::new(false);
        cpp.macros.insert(name, Macro { parameters, replacement, active });
    }

    fn undef_directive(
        &mut self, cpp: &mut Preprocessor<'i, 's>, symbols: &'i SymbolMap<lex::Kind>
    ) {
        let Token { token, .. } = self.unexpanded_token(cpp, symbols, true);
        let name = match token.kind() {
            lex::Kind::EndOfLine => { return; }
            lex::Kind::Identifier => { token.ident() }
            _ => { return self.discard_directive(cpp, symbols); }
        };
        let Token { token, .. } = self.unexpanded_token(cpp, symbols, true);
        match token.kind() {
            lex::Kind::EndOfLine => {}
            _ => { return self.discard_directive(cpp, symbols); }
        }

        cpp.macros.remove(&name);
    }

    fn line_directive(
        &mut self, cpp: &mut Preprocessor<'i, 's>, symbols: &'i SymbolMap<lex::Kind>
    ) {
        let Token { token, .. } = self.expanded_token(cpp, symbols, true);
        let _line = match token.kind() {
            lex::Kind::EndOfLine => { return; }
            lex::Kind::Number => { token }
            _ => { return self.discard_directive(cpp, symbols); }
        };

        let Token { token, .. } = self.expanded_token(cpp, symbols, true);
        let _file = match token.kind() {
            lex::Kind::EndOfLine => { return; }
            lex::Kind::String => { token }
            _ => { return self.discard_directive(cpp, symbols); }
        };
    }

    fn error_directive(
        &mut self, cpp: &mut Preprocessor<'i, 's>, symbols: &'i SymbolMap<lex::Kind>
    ) {
        self.discard_directive(cpp, symbols);
    }

    fn pragma_directive(
        &mut self, cpp: &mut Preprocessor<'i, 's>, symbols: &'i SymbolMap<lex::Kind>
    ) {
        self.discard_directive(cpp, symbols);
    }

    fn discard_directive(&mut self, cpp: &Preprocessor<'i, 's>, symbols: &'i SymbolMap<lex::Kind>) {
        loop {
            let Token { token, .. } = self.unexpanded_token(cpp, symbols, true);
            if token.kind() == lex::Kind::EndOfLine { break; }
        }
    }

    fn expanded_token(
        &mut self, cpp: &Preprocessor<'i, 's>, symbols: &'i SymbolMap<lex::Kind>, horizontal: bool
    ) -> Token<'i, 's> {
        loop {
            let mut token = self.unexpanded_token(cpp, symbols, horizontal);
            if self.try_replace_macro(cpp, symbols, &mut token, horizontal) { continue; }
            break token;
        }
    }

    fn unexpanded_token(
        &mut self, cpp: &Preprocessor<'i, 's>, symbols: &'i SymbolMap<lex::Kind>, horizontal: bool
    ) -> Token<'i, 's> {
        while let Some(&Invoked { name, begin }) = self.macros.last() {
            if begin < self.buffer.len() { break; }
            assert!(cpp.macros[&name].active.replace(false));
            self.macros.pop();
        }

        if let Some(mut token) = self.buffer.pop() {
            if self.newline { token.space.kind = lex::Shape::Newline; }
            self.offset += token.space.len;
            self.newline = false;
            self.offset += token.token.len();

            token
        } else {
            let mut space = self.tokens.whitespace(horizontal);
            if self.newline { space.kind = lex::Shape::Newline; }
            if let lex::Shape::Newline = space.kind { self.newline = true; }
            self.offset += space.len;

            if horizontal && self.newline {
                let token = lex::Token::end_of_line();
                return Token { space, token, replace: true };
            }

            let token = self.tokens.preprocessing_token(symbols, &mut self.scratch);
            self.newline = false;
            self.offset += token.len();

            Token { space, token, replace: true }
        }
    }

    fn try_unexpanded_left_paren(
        &mut self, cpp: &Preprocessor<'i, 's>, symbols: &'i SymbolMap<lex::Kind>, horizontal: bool
    ) -> bool {
        while let Some(&Invoked { name, begin }) = self.macros.last() {
            if begin < self.buffer.len() { break; }
            assert!(cpp.macros[&name].active.replace(false));
            self.macros.pop();
        }

        if let Some(&Token { ref token, .. }) = self.buffer.last() {
            if token.kind() != lex::Kind::LeftParen { return false; }

            let Token { space, token, .. } = self.buffer.pop().unwrap();
            self.offset += space.len;
            self.newline = false;
            self.offset += token.len();
        } else {
            let mut tokens = self.tokens;
            let space = tokens.whitespace(horizontal);
            let token = tokens.preprocessing_token(symbols, &mut self.scratch);

            if horizontal && space.kind == Shape::Newline { return false; }
            if token.kind() != lex::Kind::LeftParen { return false; }

            self.tokens = tokens;
            if space.kind == Shape::Newline { self.newline = true; }
            self.offset += space.len;
            self.newline = false;
            self.offset += token.len();
        }

        true
    }

    fn try_replace_macro(
        &mut self, cpp: &Preprocessor<'i, 's>, symbols: &'i SymbolMap<lex::Kind>,
        token: &mut Token<'i, 's>, horizontal: bool
    ) -> bool {
        let name = match token.token.kind() {
            lex::Kind::Identifier => { token.token.ident() }
            _ => { return false; }
        };
        let definition = match cpp.macros.get(&name) {
            Some(definition) => { definition }
            None => { return false; }
        };
        if definition.active.get() { token.replace = false; }
        if !token.replace { return false; }

        let mut tokens = Vec::default();
        let mut arguments = Vec::default();
        let mut processed = Vec::default();
        let mut variable_non_empty = false;
        if let Some(ref parameters) = definition.parameters {
            let variable = parameters.last()
                .map_or(false, |param| param.name == cpp.va_args);

            // Parse the macro arguments. Store their tokens in `tokens`, with each argument
            // `a` stored in the range `arguments[a + 0]..arguments[a + 1]`.
            if !self.try_unexpanded_left_paren(cpp, symbols, horizontal) {
                return false;
            }
            arguments.push(tokens.len());
            let mut parens = 1;
            loop {
                let mut token = self.unexpanded_token(cpp, symbols, horizontal);
                match token.token.kind() {
                    lex::Kind::EndOfFile | lex::Kind::EndOfLine => { return false; }
                    lex::Kind::Identifier => {
                        let name = token.token.ident();
                        if let Some(definition) = cpp.macros.get(&name) {
                            if definition.active.get() { token.replace = false; }
                        }
                    }
                    lex::Kind::Comma if
                        parens == 1 && (!variable || arguments.len() < parameters.len())
                    => { arguments.push(tokens.len()); continue; }
                    lex::Kind::LeftParen => { parens += 1; }
                    lex::Kind::RightParen => { parens -= 1; }
                    _ => {}
                }
                if parens == 0 { break; }

                if token.space.kind == lex::Shape::Newline {
                    token.space.kind = lex::Shape::Horizontal;
                }
                tokens.push(token);
            }
            if tokens.len() != 0 || parameters.len() != 0 { arguments.push(tokens.len()); }
            if variable && arguments.len() == parameters.len() { arguments.push(tokens.len()); }

            if arguments.len() - 1 != parameters.len() {
                return false;
            }

            // Macro-expand and stringize arguments when necessary. Store the results in `tokens`,
            // with each argument getting two entries in `processed`, similar to `arguments`.
            processed.push(tokens.len());
            for (parameter, ij) in Iterator::zip(parameters[..].iter(), arguments.windows(2)) {
                let (i, j) = (ij[0], ij[1]);

                if parameter.replaced {
                    let begin = self.buffer.len();

                    let space = lex::Space { kind: lex::Shape::None, len: 0 };
                    let token = lex::Token::end_of_file();
                    self.buffer.push(Token { space, token, replace: true });

                    self.buffer.extend(tokens[i..j].iter().rev().copied());
                    while begin < self.buffer.len() {
                        let token = self.expanded_token(cpp, symbols, horizontal);
                        tokens.push(token);
                    }

                    tokens.pop();
                }
                processed.push(tokens.len());

                if parameter.stringized {
                    tokens.push(self.stringize(cpp, symbols, &tokens[i..j]));
                }
                processed.push(tokens.len());
            }

            if variable {
                let variable = parameters.len() - 1;
                assert_eq!(parameters[variable].name, cpp.va_args);
                let p = 2 * variable + 0;
                variable_non_empty = processed[p + 0] < processed[p + 1];
            }
        }

        // Copy the macro body into `tokens`, interpolating parameters.
        let replacement = &definition.replacement[..];
        let substituted = tokens.len();
        let mut placemarker = false;
        let mut va_opt_cx = None;
        let mut paste = false;
        let mut i = 0;
        while i < replacement.len() {
            let mut token = replacement[i];
            i += 1;

            if let Some(ref parameters) = definition.parameters {
                if let Some((stringize, begin, ref mut parens)) = va_opt_cx {
                    match token.token.kind() {
                        lex::Kind::LeftParen => { *parens += 1; }
                        lex::Kind::RightParen => { *parens -= 1; }
                        _ => {}
                    }

                    if *parens > 0 && !variable_non_empty { continue; }
                    if *parens == 0 {
                        va_opt_cx = None;

                        let mut begin = begin;
                        if let Some((paste, space)) = stringize {
                            token = self.stringize(cpp, symbols, &tokens[begin..]);
                            token.space = space;

                            tokens.truncate(begin);

                            if paste {
                                begin -= 1;
                                let prev = tokens.pop().unwrap();
                                token = self.paste(cpp, symbols, &prev, &token);
                            }

                            placemarker = false;
                            tokens.push(token);
                        }

                        if tokens.len() == begin {
                            placemarker = true;

                            if paste {
                                paste = false;
                                placemarker = false;
                            }
                        }

                        continue;
                    }
                }

                let mut stringize = false;
                let space = token.space;
                if token.token.kind() == lex::Kind::Hash {
                    token = replacement[i];
                    i += 1;

                    stringize = true;
                }
                if token.token.kind() == lex::Kind::Identifier {
                    let name = token.token.ident();

                    let parameter = parameters.iter()
                        .position(move |parameter| parameter.name == name);
                    if let Some(parameter) = parameter {
                        let prev = i.checked_sub(2).and_then(|i| replacement.get(i))
                            .map_or(lex::Kind::Error, |&Token { ref token, .. }| token.kind());
                        let next = replacement.get(i)
                            .map_or(lex::Kind::Error, |&Token { ref token, .. }| token.kind());

                        let mut range = if stringize {
                            let p = 2 * parameter + 1;
                            processed[p + 0]..processed[p + 1]
                        } else if prev == lex::Kind::HashHash || next == lex::Kind::HashHash {
                            arguments[parameter + 0]..arguments[parameter + 1]
                        } else {
                            let p = 2 * parameter + 0;
                            processed[p + 0]..processed[p + 1]
                        };

                        let mut begin = tokens.len();
                        if let Some(next) = range.next() {
                            token = tokens[next];
                            token.space = space;

                            if paste {
                                paste = false;

                                begin -= 1;
                                if let Some((_, ref mut begin, _)) = va_opt_cx {
                                    if tokens.len() == *begin { *begin -= 1; }
                                }
                                let prev = tokens.pop().unwrap();
                                token = self.paste(cpp, symbols, &prev, &token);
                            }

                            placemarker = false;
                            tokens.push(token);

                            tokens.extend_from_within(range);
                        }

                        if tokens.len() == begin {
                            placemarker = true;

                            if paste {
                                paste = false;
                                placemarker = false;
                            }
                        }

                        continue;
                    } else if name == cpp.va_opt {
                        token = replacement[i];
                        i += 1;

                        assert_eq!(token.token.kind(), lex::Kind::LeftParen);

                        let stringize = if stringize { Some((paste, space)) } else { None };
                        va_opt_cx = Some((stringize, tokens.len(), 1));
                        if stringize.is_some() { paste = false; }

                        continue;
                    }
                }
            }

            if !paste && token.token.kind() == lex::Kind::HashHash {
                if placemarker {
                    placemarker = false;
                } else {
                    paste = true;
                }

                continue;
            }

            if paste {
                paste = false;

                if let Some((_, ref mut begin, _)) = va_opt_cx {
                    if tokens.len() == *begin { *begin -= 1; }
                }
                let prev = tokens.pop().unwrap();
                token = self.paste(cpp, symbols, &prev, &token);
            }

            placemarker = false;
            tokens.push(token);
        }

        let begin = self.buffer.len();
        self.macros.push(Invoked { name, begin });
        assert!(!cpp.macros[&name].active.replace(true));
        self.buffer.extend(tokens.drain(substituted..).rev());

        true
    }

    fn stringize(
        &mut self, cpp: &Preprocessor<'i, 's>, symbols: &'i SymbolMap<lex::Kind>,
        tokens: &[Token<'i, 's>]
    ) -> Token<'i, 's> {
        self.scratch.push(b'"');
        let mut first = true;
        for &Token { space, ref token, .. } in tokens {
            if !first && space.kind != lex::Shape::None { self.scratch.push(b' '); }
            first = false;

            let mut i = self.scratch.len();
            token.write_spelling(&mut self.scratch);
            if token.kind() == lex::Kind::Character || token.kind() == lex::Kind::String {
                while i < self.scratch.len() {
                    if self.scratch[i] == b'"' || self.scratch[i] == b'\\' {
                        self.scratch.insert(i, b'\\');
                        i += 1;
                    }
                    i += 1;
                }
            }
        }
        self.scratch.push(b'"');

        let mut string = cpp.source.lexer_for_buffer(&self.scratch[..]);
        let bytes = string.bytes();
        self.scratch.clear();

        let space = lex::Space { kind: lex::Shape::None, len: 0 };
        let mut token = string.preprocessing_token(symbols, &mut self.scratch);
        let end = string.preprocessing_token(symbols, &mut self.scratch);
        if end.kind() != lex::Kind::EndOfFile {
            token = lex::Token::error(bytes);
        }

        Token { space, token, replace: true }
    }

    fn paste(
        &mut self, cpp: &Preprocessor<'i, 's>, symbols: &'i SymbolMap<lex::Kind>,
        prev: &Token<'i, 's>, next: &Token<'i, 's>
    ) -> Token<'i, 's> {
        prev.token.write_spelling(&mut self.scratch);
        next.token.write_spelling(&mut self.scratch);

        let mut tokens = cpp.source.lexer_for_buffer(&self.scratch[..]);
        let bytes = tokens.bytes();
        self.scratch.clear();

        let space = prev.space;
        let mut token = tokens.preprocessing_token(symbols, &mut self.scratch);
        let end = tokens.preprocessing_token(symbols, &mut self.scratch);
        if end.kind() != lex::Kind::EndOfFile {
            token = lex::Token::error(bytes);
        }

        Token { space, token, replace: true }
    }
}

#[cfg(test)]
mod tests {
    use std::{ptr, slice};
    use std::alloc::{Layout, handle_alloc_error};
    use crate::lex;
    use crate::symbols::SymbolMap;
    use super::{Preprocessor, Source};

    #[derive(Default)]
    struct Buffers { arena: quickdry::Arena }

    impl Source for Buffers {
        fn lexer_for_header(&self, _: bool, _: &[u8]) -> Option<lex::Tokens<'_>> { None }
        fn lexer_for_buffer(&self, buffer: &[u8]) -> lex::Tokens<'_> {
            unsafe {
                let layout = Layout::from_size_align_unchecked(buffer.len() + 3, 1);
                let data = self.arena.alloc(layout);
                if data == ptr::null_mut() { handle_alloc_error(layout); }

                ptr::copy_nonoverlapping(buffer.as_ptr(), data, buffer.len());
                ptr::write_bytes(data.add(buffer.len()), 0, 3);

                let bytes = slice::from_raw_parts(data, buffer.len() + 3);
                lex::Tokens::from_bytes_with_padding(bytes).unwrap()
            }
        }
    }

    #[test]
    fn hello() {
        let symbols = &mut SymbolMap::default();
        lex::Tokens::alternative_tokens(symbols);

        let tokens = lex::Tokens::from_bytes_with_padding(b"\
if (x < 3) {
    printf(\"hello world\");
}
\0\0\0").unwrap();

        let source = &Buffers::default();
        let cpp = &mut Preprocessor::new(symbols, source);
        let tokens = &mut cpp.tokens(tokens);

        let result = [
            lex::Kind::Identifier,
            lex::Kind::LeftParen,
            lex::Kind::Identifier,
            lex::Kind::Lt,
            lex::Kind::Number,
            lex::Kind::RightParen,
            lex::Kind::LeftBrace,
            lex::Kind::Identifier,
            lex::Kind::LeftParen,
            lex::Kind::String,
            lex::Kind::RightParen,
            lex::Kind::Semi,
            lex::Kind::RightBrace,
            lex::Kind::EndOfFile,
        ];
        for kind in result {
            let token = tokens.preprocessed_token(cpp, symbols);
            assert_eq!(token.kind(), kind);
        }
    }

    #[test]
    fn conditional() {
        let symbols = &mut SymbolMap::default();
        lex::Tokens::alternative_tokens(symbols);

        let tokens = lex::Tokens::from_bytes_with_padding(b"\
#define A 1
#define B 0
#if A == B
abc
#elif defined(B)
#ifndef C
def
#else
ghi
#endif
#endif
\0\0\0").unwrap();

        let source = &Buffers::default();
        let cpp = &mut Preprocessor::new(symbols, source);
        let tokens = &mut cpp.tokens(tokens);

        let result = [
            lex::Kind::Identifier,
            lex::Kind::EndOfFile,
        ];
        for kind in result {
            let token = tokens.preprocessed_token(cpp, symbols);
            assert_eq!(token.kind(), kind);
        }
    }

    #[test]
    fn define() {
        let symbols = &mut SymbolMap::default();
        lex::Tokens::alternative_tokens(symbols);

        let tokens = lex::Tokens::from_bytes_with_padding(
b"#define OBJ0 3
int x = OBJ0;

#define OBJ1 3 * 5
int y = OBJ1;

#define FN0(X) X + 8
int z = FN0(OBJ1);

#define FN1(X, Y) ((X) <= (Y) ? (X) : (Y))
int w = FN1(OBJ1, FN0(OBJ0));

\0\0\0"
        ).unwrap();

        let source = &Buffers::default();
        let cpp = &mut Preprocessor::new(symbols, source);
        let tokens = &mut cpp.tokens(tokens);

        let result = [
            // int x = 3;
            lex::Kind::Identifier,
            lex::Kind::Identifier,
            lex::Kind::Eq,
            lex::Kind::Number,
            lex::Kind::Semi,

            // int y = 3 * 5;
            lex::Kind::Identifier,
            lex::Kind::Identifier,
            lex::Kind::Eq,
            lex::Kind::Number,
            lex::Kind::Star,
            lex::Kind::Number,
            lex::Kind::Semi,

            // int z = 3 * 5 + 8;
            lex::Kind::Identifier,
            lex::Kind::Identifier,
            lex::Kind::Eq,
            lex::Kind::Number,
            lex::Kind::Star,
            lex::Kind::Number,
            lex::Kind::Plus,
            lex::Kind::Number,
            lex::Kind::Semi,

            // int w = ((3 * 5) <= (3 + 8) ? (3 * 5) : (3 + 8));
            lex::Kind::Identifier,
            lex::Kind::Identifier,
            lex::Kind::Eq,
            lex::Kind::LeftParen,
                lex::Kind::LeftParen,
                lex::Kind::Number,
                lex::Kind::Star,
                lex::Kind::Number,
                lex::Kind::RightParen,

                lex::Kind::LtEq,

                lex::Kind::LeftParen,
                lex::Kind::Number,
                lex::Kind::Plus,
                lex::Kind::Number,
                lex::Kind::RightParen,

                lex::Kind::Question,

                lex::Kind::LeftParen,
                lex::Kind::Number,
                lex::Kind::Star,
                lex::Kind::Number,
                lex::Kind::RightParen,

                lex::Kind::Colon,

                lex::Kind::LeftParen,
                lex::Kind::Number,
                lex::Kind::Plus,
                lex::Kind::Number,
                lex::Kind::RightParen,
            lex::Kind::RightParen,
            lex::Kind::Semi,

            lex::Kind::EndOfFile,
        ];
        for kind in result {
            let token = tokens.preprocessed_token(cpp, symbols);
            assert_eq!(token.kind(), kind);
        }
    }

    #[test]
    fn expansion() {
        let symbols = &mut SymbolMap::default();
        lex::Tokens::alternative_tokens(symbols);

        let tokens = lex::Tokens::from_bytes_with_padding(
b"#define LPAREN() (
#define G(Q) 42
#define F(R, X, ...)  __VA_OPT__(G R X) )
int x = F(LPAREN(), 0, <:-);    // replaced by int x = 42;
\0\0\0"
        ).unwrap();

        let source = &Buffers::default();
        let cpp = &mut Preprocessor::new(symbols, source);
        let tokens = &mut cpp.tokens(tokens);

        let result = [
            // int x = 42;
            lex::Kind::Identifier,
            lex::Kind::Identifier,
            lex::Kind::Eq,
            lex::Kind::Number,
            lex::Kind::Semi,

            lex::Kind::EndOfFile,
        ];
        for kind in result {
            let token = tokens.preprocessed_token(cpp, symbols);
            assert_eq!(token.kind(), kind);
        }
    }

    #[test]
    fn va_args() {
        let symbols = &mut SymbolMap::default();
        lex::Tokens::alternative_tokens(symbols);

        let tokens = lex::Tokens::from_bytes_with_padding(
b"#define debug(...) fprintf(stderr, __VA_ARGS__)
#define showlist(...) puts(#__VA_ARGS__)
#define report(test, ...) ((test) ? puts(#test) : printf(__VA_ARGS__))
debug(\"Flag\");
debug(\"X = %d\\n\", x);
showlist(The first, second, and third items.);
report(x>y, \"x is %d but y is %d\", x, y);
\0\0\0"
        ).unwrap();

        let source = &Buffers::default();
        let cpp = &mut Preprocessor::new(symbols, source);
        let tokens = &mut cpp.tokens(tokens);

        let result = [
            // fprintf(stderr, "Flag");
            lex::Kind::Identifier,
            lex::Kind::LeftParen,
            lex::Kind::Identifier,
            lex::Kind::Comma,
            lex::Kind::String,
            lex::Kind::RightParen,
            lex::Kind::Semi,

            // fprintf(stderr, "X = %d\n", x);
            lex::Kind::Identifier,
            lex::Kind::LeftParen,
            lex::Kind::Identifier,
            lex::Kind::Comma,
            lex::Kind::String,
            lex::Kind::Comma,
            lex::Kind::Identifier,
            lex::Kind::RightParen,
            lex::Kind::Semi,

            // puts("The first, second, and third items.");
            lex::Kind::Identifier,
            lex::Kind::LeftParen,
            lex::Kind::String,
            lex::Kind::RightParen,
            lex::Kind::Semi,

            // ((x>y) ? puts("x>y") : printf("x is %d but y is %d", x, y));
            lex::Kind::LeftParen,
            lex::Kind::LeftParen,
            lex::Kind::Identifier,
            lex::Kind::Gt,
            lex::Kind::Identifier,
            lex::Kind::RightParen,

            lex::Kind::Question,

            lex::Kind::Identifier,
            lex::Kind::LeftParen,
            lex::Kind::String,
            lex::Kind::RightParen,

            lex::Kind::Colon,

            lex::Kind::Identifier,
            lex::Kind::LeftParen,
            lex::Kind::String,
            lex::Kind::Comma,
            lex::Kind::Identifier,
            lex::Kind::Comma,
            lex::Kind::Identifier,
            lex::Kind::RightParen,

            lex::Kind::RightParen,
            lex::Kind::Semi,

            lex::Kind::EndOfFile,
        ];
        for kind in result {
            let token = tokens.preprocessed_token(cpp, symbols);
            assert_eq!(token.kind(), kind);
        }
    }

    #[test]
    fn va_opt() {
        let symbols = &mut SymbolMap::default();
        lex::Tokens::alternative_tokens(symbols);

        let tokens = lex::Tokens::from_bytes_with_padding(
b"#define F(...)           f(0 __VA_OPT__(,) __VA_ARGS__)
#define G(X, ...)        f(0, X __VA_OPT__(,) __VA_ARGS__)
#define SDEF(sname, ...) S sname __VA_OPT__(= { __VA_ARGS__ })
#define EMP

F(a, b, c)          // replaced by f(0, a, b, c)
F()                 // replaced by f(0)
F(EMP)              // replaced by f(0)

G(a, b, c)          // replaced by f(0, a, b, c)
G(a, )              // replaced by f(0, a)
G(a)                // replaced by f(0, a)

SDEF(foo);          // replaced by S foo;
SDEF(bar, 1, 2);    // replaced by S bar = { 1, 2 };

#define H1(X, ...) X __VA_OPT__(##) __VA_ARGS__ // error: ## may not appear at
                                                // the beginning of a replacement list ([cpp.concat])

#define H2(X, Y, ...) __VA_OPT__(X ## Y,) __VA_ARGS__
H2(a, b, c, d)      // replaced by ab, c, d

#define H3(X, ...) #__VA_OPT__(X##X X##X)
H3(, 0)             // replaced by \"\"

#define H4(X, ...) __VA_OPT__(a X ## X) ## b
H4(, 1)             // replaced by a b

#define H5A(...) __VA_OPT__()/**/__VA_OPT__()
#define H5B(X) a ## X ## b
#define H5C(X) H5B(X)
H5C(H5A())          // replaced by ab
\0\0\0"
        ).unwrap();

        let source = &Buffers::default();
        let cpp = &mut Preprocessor::new(symbols, source);
        let tokens = &mut cpp.tokens(tokens);

        let result = [
            // f(0, a, b, c)
            lex::Kind::Identifier,
            lex::Kind::LeftParen,
            lex::Kind::Number,
            lex::Kind::Comma,
            lex::Kind::Identifier,
            lex::Kind::Comma,
            lex::Kind::Identifier,
            lex::Kind::Comma,
            lex::Kind::Identifier,
            lex::Kind::RightParen,

            // f(0)
            lex::Kind::Identifier,
            lex::Kind::LeftParen,
            lex::Kind::Number,
            lex::Kind::RightParen,

            // f(0)
            lex::Kind::Identifier,
            lex::Kind::LeftParen,
            lex::Kind::Number,
            lex::Kind::RightParen,

            // f(0, a, b, c)
            lex::Kind::Identifier,
            lex::Kind::LeftParen,
            lex::Kind::Number,
            lex::Kind::Comma,
            lex::Kind::Identifier,
            lex::Kind::Comma,
            lex::Kind::Identifier,
            lex::Kind::Comma,
            lex::Kind::Identifier,
            lex::Kind::RightParen,

            // f(0, a)
            lex::Kind::Identifier,
            lex::Kind::LeftParen,
            lex::Kind::Number,
            lex::Kind::Comma,
            lex::Kind::Identifier,
            lex::Kind::RightParen,

            // f(0, a)
            lex::Kind::Identifier,
            lex::Kind::LeftParen,
            lex::Kind::Number,
            lex::Kind::Comma,
            lex::Kind::Identifier,
            lex::Kind::RightParen,

            // S foo;
            lex::Kind::Identifier,
            lex::Kind::Identifier,
            lex::Kind::Semi,

            // S foo = { 1, 2 };
            lex::Kind::Identifier,
            lex::Kind::Identifier,
            lex::Kind::Eq,
            lex::Kind::LeftBrace,
            lex::Kind::Number,
            lex::Kind::Comma,
            lex::Kind::Number,
            lex::Kind::RightBrace,
            lex::Kind::Semi,

            // ab, c, d
            lex::Kind::Identifier,
            lex::Kind::Comma,
            lex::Kind::Identifier,
            lex::Kind::Comma,
            lex::Kind::Identifier,

            // ""
            lex::Kind::String,

            // a b
            lex::Kind::Identifier,
            lex::Kind::Identifier,

            // ab
            lex::Kind::Identifier,

            lex::Kind::EndOfFile,
        ];
        for kind in result {
            let token = tokens.preprocessed_token(cpp, symbols);
            assert_eq!(token.kind(), kind);
        }
    }

    #[test]
    fn paste() {
        let symbols = &mut SymbolMap::default();
        lex::Tokens::alternative_tokens(symbols);

        let tokens = lex::Tokens::from_bytes_with_padding(
b"#define str(s)      # s
#define xstr(s)     str(s)
#define debug(s, t) printf(\"x\" # s \"= %d, x\" # t \"= %s\", \\
               x ## s, x ## t)
#define INCFILE(n)  vers ## n
#define glue(a, b)  a ## b
#define xglue(a, b) glue(a, b)
#define HIGHLOW     \"hello\"
#define LOW         LOW \", world\"

debug(1, 2);
fputs(str(strncmp(\"abc\\0d\", \"abc\", '\\4')        // this goes away
    == 0) str(: @\\n), s);
#include xstr(INCFILE(2).h)
glue(HIGH, LOW);
xglue(HIGH, LOW)
\0\0\0"
        ).unwrap();

        let source = &Buffers::default();
        let cpp = &mut Preprocessor::new(symbols, source);
        let tokens = &mut cpp.tokens(tokens);

        let result = [
            // printf("x" "1" "= %d, x" "2" "= %s", x1, x2);
            lex::Kind::Identifier,
            lex::Kind::LeftParen,
            lex::Kind::String,
            lex::Kind::String,
            lex::Kind::String,
            lex::Kind::String,
            lex::Kind::String,
            lex::Kind::Comma,
            lex::Kind::Identifier,
            lex::Kind::Comma,
            lex::Kind::Identifier,
            lex::Kind::RightParen,
            lex::Kind::Semi,

            // fputs("strncmp(\"abc\\0d\", \"abc\", '\\4') == 0" ": @\n", s);
            lex::Kind::Identifier,
            lex::Kind::LeftParen,
            lex::Kind::String,
            lex::Kind::String,
            lex::Kind::Comma,
            lex::Kind::Identifier,
            lex::Kind::RightParen,
            lex::Kind::Semi,

            // "hello";
            lex::Kind::String,
            lex::Kind::Semi,

            // "hello" ", world"
            lex::Kind::String,
            lex::Kind::String,

            lex::Kind::EndOfFile,
        ];
        for kind in result {
            let token = tokens.preprocessed_token(cpp, symbols);
            assert_eq!(token.kind(), kind);
        }
    }

    #[test]
    fn rescan() {
        let symbols = &mut SymbolMap::default();
        lex::Tokens::alternative_tokens(symbols);

        let tokens = lex::Tokens::from_bytes_with_padding(
b"#define x       3
#define f(a)    f(x * (a))
#undef  x
#define x       2
#define g       f
#define z       z[0]
#define h       g(~
#define m(a)    a(w)
#define w       0,1
#define t(a)    a
#define p()     int
#define q(x)    x
#define r(x,y)  x ## y
#define str(x)  # x

f(y+1) + f(f(z)) % t(t(g)(0) + t)(1);
g(x+(3,4)-w) | h 5) & m
    (f)^m(m);
p() i[q()] = { q(1), r(2,3), r(4,), r(,5), r(,) };
char c[2][6] = { str(hello), str() };
\0\0\0"
        ).unwrap();

        let source = &Buffers::default();
        let cpp = &mut Preprocessor::new(symbols, source);
        let tokens = &mut cpp.tokens(tokens);

        let result = [
            // f(2 * (y+1)) + f(2 * (f(2 * (z[0])))) % f(2 * (0)) + t(1);
            lex::Kind::Identifier,
            lex::Kind::LeftParen,
            lex::Kind::Number,
            lex::Kind::Star,
            lex::Kind::LeftParen,
            lex::Kind::Identifier,
            lex::Kind::Plus,
            lex::Kind::Number,
            lex::Kind::RightParen,
            lex::Kind::RightParen,

            lex::Kind::Plus,

            lex::Kind::Identifier,
            lex::Kind::LeftParen,
            lex::Kind::Number,
            lex::Kind::Star,
                lex::Kind::LeftParen,
                lex::Kind::Identifier,
                lex::Kind::LeftParen,
                lex::Kind::Number,
                lex::Kind::Star,
                    lex::Kind::LeftParen,
                    lex::Kind::Identifier,
                    lex::Kind::LeftBracket,
                    lex::Kind::Number,
                    lex::Kind::RightBracket,
                    lex::Kind::RightParen,
                lex::Kind::RightParen,
                lex::Kind::RightParen,
            lex::Kind::RightParen,

            lex::Kind::Percent,

            lex::Kind::Identifier,
            lex::Kind::LeftParen,
            lex::Kind::Number,
            lex::Kind::Star,
            lex::Kind::LeftParen,
            lex::Kind::Number,
            lex::Kind::RightParen,
            lex::Kind::RightParen,

            lex::Kind::Plus,

            lex::Kind::Identifier,
            lex::Kind::LeftParen,
            lex::Kind::Number,
            lex::Kind::RightParen,

            lex::Kind::Semi,

            // f(2 * (2+(3,4)-0,1)) | f(2 * (~ 5)) & f(2 * (0,1))^m(0,1);
            lex::Kind::Identifier,
            lex::Kind::LeftParen,
            lex::Kind::Number,
            lex::Kind::Star,
            lex::Kind::LeftParen,
            lex::Kind::Number,
            lex::Kind::Plus,
            lex::Kind::LeftParen,
            lex::Kind::Number,
            lex::Kind::Comma,
            lex::Kind::Number,
            lex::Kind::RightParen,
            lex::Kind::Minus,
            lex::Kind::Number,
            lex::Kind::Comma,
            lex::Kind::Number,
            lex::Kind::RightParen,
            lex::Kind::RightParen,

            lex::Kind::Pipe,

            lex::Kind::Identifier,
            lex::Kind::LeftParen,
            lex::Kind::Number,
            lex::Kind::Star,
                lex::Kind::LeftParen,
                lex::Kind::Tilde,
                lex::Kind::Number,
                lex::Kind::RightParen,
            lex::Kind::RightParen,

            lex::Kind::Amp,

            lex::Kind::Identifier,
            lex::Kind::LeftParen,
            lex::Kind::Number,
            lex::Kind::Star,
            lex::Kind::LeftParen,
            lex::Kind::Number,
            lex::Kind::Comma,
            lex::Kind::Number,
            lex::Kind::RightParen,
            lex::Kind::RightParen,

            lex::Kind::Caret,

            lex::Kind::Identifier,
            lex::Kind::LeftParen,
            lex::Kind::Number,
            lex::Kind::Comma,
            lex::Kind::Number,
            lex::Kind::RightParen,

            lex::Kind::Semi,

            // int i[] = { 1, 23, 4, 5, };
            lex::Kind::Identifier,
            lex::Kind::Identifier,
            lex::Kind::LeftBracket,
            lex::Kind::RightBracket,
            lex::Kind::Eq,
            lex::Kind::LeftBrace,
            lex::Kind::Number,
            lex::Kind::Comma,
            lex::Kind::Number,
            lex::Kind::Comma,
            lex::Kind::Number,
            lex::Kind::Comma,
            lex::Kind::Number,
            lex::Kind::Comma,
            lex::Kind::RightBrace,
            lex::Kind::Semi,

            // char c[2][6] = { "hello", "" };
            lex::Kind::Identifier,
            lex::Kind::Identifier,
            lex::Kind::LeftBracket,
            lex::Kind::Number,
            lex::Kind::RightBracket,
            lex::Kind::LeftBracket,
            lex::Kind::Number,
            lex::Kind::RightBracket,
            lex::Kind::Eq,
            lex::Kind::LeftBrace,
            lex::Kind::String,
            lex::Kind::Comma,
            lex::Kind::String,
            lex::Kind::RightBrace,
            lex::Kind::Semi,

            lex::Kind::EndOfFile,
        ];
        for kind in result {
            let token = tokens.preprocessed_token(cpp, symbols);
            assert_eq!(token.kind(), kind);
        }
    }

    #[test]
    fn va_opt_paste() {
        let symbols = &mut SymbolMap::default();
        lex::Tokens::alternative_tokens(symbols);

        let tokens = lex::Tokens::from_bytes_with_padding(
b"#define P0(X, Y, ...) a ## __VA_OPT__(X X) ## __VA_OPT__(Y Y) ## b
P0(, , 0);

#define P1(X, Y, ...) __VA_OPT__(a ## X) ## __VA_OPT__(Y ## b)
P1(, , 0);

#define P2(X, Y, ...) __VA_OPT__(a X) ## __VA_OPT__(Y b)
P2(, , 0);

#define P3(X, Y) a ## X ## Y ## b
P3(, );

#define F(X, Y, ...) X ## # Y
F(u, 0);

#define G(X, Y) H(X ## Y)
#define H(X) # X
G(1, 2);

#define I(X, ...) a ## __VA_OPT__(X) a
I(, 0);

#define J a ## ## b
J;

\0\0\0"
        ).unwrap();

        let source = &Buffers::default();
        let cpp = &mut Preprocessor::new(symbols, source);
        let tokens = &mut cpp.tokens(tokens);

        let result = [
            // a b;
            lex::Kind::Identifier,
            lex::Kind::Identifier,
            lex::Kind::Semi,

            // ab;
            lex::Kind::Identifier,
            lex::Kind::Semi,

            // a b;
            lex::Kind::Identifier,
            lex::Kind::Identifier,
            lex::Kind::Semi,

            // ab;
            lex::Kind::Identifier,
            lex::Kind::Semi,

            // u"0";
            lex::Kind::String,
            lex::Kind::Semi,

            // "12";
            lex::Kind::String,
            lex::Kind::Semi,

            // a a;
            lex::Kind::Identifier,
            lex::Kind::Identifier,
            lex::Kind::Semi,

            // a## b;
            lex::Kind::Error,
            lex::Kind::Identifier,
            lex::Kind::Semi,

            lex::Kind::EndOfFile,
        ];
        for kind in result {
            let token = tokens.preprocessed_token(cpp, symbols);
            assert_eq!(token.kind(), kind);
        }
    }
}
