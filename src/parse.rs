use crate::grammar;
use crate::earley::{Analysis, Parse};
use crate::lex;
use crate::cpp::Preprocessor;

/// Translation phase 7 - convert pp-tokens to tokens and parse
pub fn parse<'i, 'a, 's>(
    analysis: &'a Analysis,
    cpp: &mut Preprocessor<'i, 's>, symbols: &'i lex::SymbolMap, tokens: lex::Tokens<'s>,
) -> Parse<'a> {
    let tokens = &mut cpp.tokens(tokens);
    let scratch = &mut Vec::default();
    let mut parse = Parse::from_analysis(&analysis);
    loop {
        let token = tokens.preprocessed_token(cpp, symbols);
        let terminal = match token.kind() {
            lex::Kind::EndOfFile => { break; }

            lex::Kind::Identifier => {
                let &(_, kind) = token.ident().value();
                kind as usize
            }

            lex::Kind::Number => {
                let number = token.number(symbols, scratch).unwrap();
                let float = number.float();
                let suffix = match number.size() {
                    Some(lex::Size::User(_)) => { true }
                    _ => { false }
                };
                match (float, suffix) {
                    (false, false) => { lex::Kind::IntegerLiteral as usize }
                    (false, true) => { lex::Kind::UserIntegerLiteral as usize }
                    (true, false) => { lex::Kind::FloatLiteral as usize }
                    (true, true) => { lex::Kind::UserFloatLiteral as usize }
                }
            }
            lex::Kind::Character => {
                let character = token.character(symbols, scratch);
                match character.suffix() {
                    None => { lex::Kind::CharacterLiteral as usize }
                    Some(_) => { lex::Kind::UserCharacterLiteral as usize }
                }
            }
            lex::Kind::String => {
                let string = token.string(symbols, scratch);
                match string.suffix() {
                    None => { lex::Kind::StringLiteral as usize }
                    Some(_) => { lex::Kind::UserStringLiteral as usize }
                }
            }

            lex::Kind::GtGt => {
                parse.parse(lex::Kind::Gt as usize);
                lex::Kind::AdjacentGt as usize
            }

            kind => { kind as usize }
        };
        parse.parse(terminal);
    }

    parse
}

/// Assign kinds to keywords.
///
/// Should be called before interning anything else that might overlap.
pub fn keywords(read: &grammar::Read<'_>, symbols: &lex::SymbolMap) {
    for &k in &read.keywords[..] {
        let keyword = read.terminals[k];
        let Ok(kind) = u8::try_from(k) else { unreachable!() };
        symbols.intern(keyword.key(), (lex::Kind::Identifier, kind));
    }
}

#[cfg(test)]
mod tests {
    use crate::symbols::SymbolMap;
    use crate::grammar::{Read, Grammar, GrammarBuf};
    use crate::earley::{Analysis, Parse};
    use crate::lex;
    use crate::cpp::{Preprocessor, ScratchBuffers};
    use crate::syntax::Syntax;
    use super::parse;

    fn variable(grammar_symbols: &SymbolMap<()>, read: &Read<'_>, s: &str) -> usize {
        let v = grammar_symbols.intern(s.as_bytes(), ());
        let v = read.variables.get_index_of(&v).unwrap();
        read.terminals.len() + v
    }

    fn optional(grammar_symbols: &SymbolMap<()>, read: &Read<'_>, s: &str) -> usize {
        let v = grammar_symbols.intern(s.as_bytes(), ());
        let v = read.optionals.get_index_of(&v).unwrap();
        read.terminals.len() + read.variables.len() + v
    }

    #[test]
    fn hello_world() {
        let grammar_symbols = &SymbolMap::default();
        let mut buf = GrammarBuf::default();
        let (read, grammar) = Grammar::build(&Syntax, grammar_symbols, &mut buf);
        let analysis = &Analysis::from_grammar(&grammar);

        let symbols = &SymbolMap::default();
        lex::Tokens::alternative_tokens(symbols);

        let source = &ScratchBuffers::default();
        let tokens = lex::Tokens::from_bytes_with_padding(b"\
int main(int argc, char *argv[]) {
    printf(\"hello world\\n\");
}
\0\0\0").unwrap();

        let cpp = &mut Preprocessor::new(symbols, source);
        let parse = parse(analysis, cpp, symbols, tokens);

        let Parse { ref sets, ref nodes, ref packs, .. } = parse;
        let Grammar { terminals, .. } = grammar;

        let i = sets[sets.len() - 1];
        let s = i + nodes[i..].iter()
            .position(|&(slot, start, _)| (slot, start) == (!terminals, 0))
            .expect("completed start symbol");

        let mut p = s;
        let spine = [
            optional(grammar_symbols, &read, "declaration-seq"),
            variable(grammar_symbols, &read, "declaration-seq"),
            variable(grammar_symbols, &read, "declaration"),
            variable(grammar_symbols, &read, "name-declaration"),
            variable(grammar_symbols, &read, "function-definition"),
        ];
        for v in spine {
            let [(i, j)] = packs[nodes[p + 0].2..nodes[p + 1].2] else { panic!("ambiguous") };
            assert_eq!(i, !0);
            assert_eq!((nodes[j].0, nodes[j].1), (!v, 0));
            p = j;
        }

        let attribute_specifier_seq_opt = optional(grammar_symbols, &read, "attribute-specifier-seq");
        let decl_specifier_seq_opt = optional(grammar_symbols, &read, "decl-specifier-seq");
        let declarator = variable(grammar_symbols, &read, "declarator");
        let virt_specifier_seq_opt = optional(grammar_symbols, &read, "virt-specifier-seq");
        let function_body = variable(grammar_symbols, &read, "function-body");

        let [(l, m)] = packs[nodes[p + 0].2..nodes[p + 1].2] else { panic!("ambiguous") };
        let [(k, l)] = packs[nodes[l + 0].2..nodes[l + 1].2] else { panic!("ambiguous") };
        let [(j, k)] = packs[nodes[k + 0].2..nodes[k + 1].2] else { panic!("ambiguous") };
        let [(i, j)] = packs[nodes[j + 0].2..nodes[j + 1].2] else { panic!("ambiguous") };
        assert_eq!((nodes[i].0, nodes[i].1), (!attribute_specifier_seq_opt, 0));
        assert_eq!((nodes[j].0, nodes[j].1), (!decl_specifier_seq_opt, 0));
        assert_eq!((nodes[k].0, nodes[k].1), (!declarator, 1));
        assert_eq!((nodes[l].0, nodes[l].1), (!virt_specifier_seq_opt, 12));
        assert_eq!((nodes[m].0, nodes[m].1), (!function_body, 12));
    }
}
