use crate::symbols::SymbolMap;
use crate::grammar;
use crate::earley::{Analysis, Parse};
use crate::lex;
use crate::cpp::Preprocessor;

/// Translation phase 7 - convert pp-tokens to tokens and parse
pub fn parse<'i, 'a, 's>(
    grammar_symbols: &'i SymbolMap<()>, read: &grammar::Read<'i>, analysis: &'a Analysis,
    cpp: &mut Preprocessor<'i, 's>, symbols: &'i SymbolMap<lex::Kind>, tokens: lex::Tokens<'s>,
) -> Parse<'a> {
    let tokens = &mut cpp.tokens(tokens);
    let mut parse = Parse::from_analysis(&analysis);
    loop {
        // TODO: Don't just re-intern things to convert to syntax's idea of token kinds.
        // This is slightly tricky because the preprocessor needs to treat keywords as identifiers.
        let token = tokens.preprocessed_token(cpp, symbols);
        let terminal = match token.kind() {
            lex::Kind::EndOfFile => { break; }
            lex::Kind::Identifier => {
                let symbol = grammar_symbols.intern(token.ident().key(), ());
                if read.terminals.contains(&symbol) {
                    std::str::from_utf8(token.ident().key()).unwrap()
                } else {
                    "identifier"
                }
            }
            lex::Kind::Number => { "integer-literal" }
            lex::Kind::Character => { "character-literal" }
            lex::Kind::String => { "string-literal" }
            lex::Kind::LeftBrace => { "{" }
            lex::Kind::RightBrace => { "}" }
            lex::Kind::LeftBracket => { "[" }
            lex::Kind::RightBracket => { "]" }
            lex::Kind::LeftParen => { "(" }
            lex::Kind::RightParen => { ")" }
            lex::Kind::Semi => { ";" }
            lex::Kind::Colon => { ":" }
            lex::Kind::Ellipsis => { "..." }
            lex::Kind::Question => { "?" }
            lex::Kind::ColonColon => { "::" }
            lex::Kind::Dot => { "." }
            lex::Kind::DotStar => { ".*" }
            lex::Kind::Arrow => { "->" }
            lex::Kind::ArrowStar => { "->*" }
            lex::Kind::Tilde => { "~" }
            lex::Kind::Exclaim => { "!" }
            lex::Kind::Plus => { "+" }
            lex::Kind::Minus => { "-" }
            lex::Kind::Star => { "*" }
            lex::Kind::Slash => { "/" }
            lex::Kind::Percent => { "%" }
            lex::Kind::Caret => { "^" }
            lex::Kind::Amp => { "&" }
            lex::Kind::Pipe => { "|" }
            lex::Kind::Eq => { "=" }
            lex::Kind::PlusEq => { "+=" }
            lex::Kind::MinusEq => { "-=" }
            lex::Kind::StarEq => { "*=" }
            lex::Kind::SlashEq => { "/=" }
            lex::Kind::PercentEq => { "%=" }
            lex::Kind::CaretEq => { "^=" }
            lex::Kind::AmpEq => { "%=" }
            lex::Kind::PipeEq => { "|=" }
            lex::Kind::EqEq => { "==" }
            lex::Kind::ExclaimEq => { "!=" }
            lex::Kind::Lt => { "<" }
            lex::Kind::Gt => { ">" }
            lex::Kind::LtEq => { "<=" }
            lex::Kind::GtEq => { ">=" }
            lex::Kind::LtEqGt => { "<=>" }
            lex::Kind::AmpAmp => { "&&" }
            lex::Kind::PipePipe => { "||" }
            lex::Kind::LtLt => { "<<" }
            lex::Kind::GtGt => { ">>" }
            lex::Kind::LtLtEq => { "<<=" }
            lex::Kind::GtGtEq => { ">>=" }
            lex::Kind::PlusPlus => { "++" }
            lex::Kind::MinusMinus => { "--" }
            lex::Kind::Comma => { "," }
            _ => { unreachable!() }
        };
        let terminal = grammar_symbols.intern(terminal.as_bytes(), ());
        let terminal = read.terminals.get_index_of(&terminal).unwrap();
        parse.parse(terminal);
    }

    parse
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
        let parse = parse(grammar_symbols, &read, analysis, cpp, symbols, tokens);

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
