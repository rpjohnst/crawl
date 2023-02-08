use std::error::Error;
use std::io::{self, Read};
use crawl::symbols::SymbolMap;
use crawl::grammar::{Grammar, GrammarBuf};
use crawl::earley::{Analysis, Parse};
use crawl::lex;
use crawl::cpp::{Preprocessor, ScratchBuffers};
use crawl::syntax::Syntax;
use crawl::parse::parse;

fn main() -> Result<(), Box<dyn Error>> {
    let grammar_symbols = &SymbolMap::default();
    let mut buf = GrammarBuf::default();
    let (read, grammar) = Grammar::build(&Syntax, grammar_symbols, &mut buf);
    let analysis = &Analysis::from_grammar(&grammar);

    let mut text = Vec::default();
    io::stdin().read_to_end(&mut text)?;
    text.extend_from_slice(b"\0\0\0");

    let symbols = &SymbolMap::default();
    lex::Tokens::alternative_tokens(symbols);

    let source = &ScratchBuffers::default();
    let tokens = lex::Tokens::from_bytes_with_padding(&text[..]).unwrap();

    let cpp = &mut Preprocessor::new(symbols, source);
    let parse = parse(grammar_symbols, &read, analysis, cpp, symbols, tokens);

    let Parse { ref sets, ref nodes, ref packs, .. } = parse;
    let Grammar { terminals, .. } = grammar;

    let i = sets[sets.len() - 1];
    let s = i + nodes[i..].iter()
        .position(|&(slot, start, _)| (slot, start) == (!terminals, 0))
        .expect("completed start symbol");

    println!("digraph {{");
    let mut stack = Vec::from_iter([s]);
    let mut visit = Vec::from_iter(std::iter::repeat(false).take(nodes.len()));
    let mut scratch = Vec::default();
    while let Some(n) = stack.pop() {
        if std::mem::replace(&mut visit[n], true) { continue; }
        let (slot, start, _) = nodes[n];
        if (slot as isize) < 0 {
            if !slot < read.terminals.len() {
                scratch.extend(read.terminals[!slot].key());
            } else if !slot < read.terminals.len() + read.variables.len() {
                scratch.extend(read.variables[!slot - read.terminals.len()].key());
            } else {
                scratch.extend(read.optionals[!slot - read.terminals.len() - read.variables.len()].key());
                scratch.extend(b"?");
            };
            let name = std::str::from_utf8(&scratch[..]).unwrap();
            println!(r#"  n{n} [label="{name} {start}", ordering=out]"#);
            scratch.clear();
        } else {
            println!(r#"  n{n} [label="", ordering=out]"#);
        }
        for (p, &(i, j)) in packs[nodes[n + 0].2..nodes[n + 1].2].iter().enumerate() {
            let p = nodes[n].2 + p;
            println!(r#"  p{p} [label="", shape=point, ordering=out]"#);
            println!("  n{n} -> p{p}");
            if i != !0 { println!("  p{p} -> n{i}"); stack.push(i); }
            if j != !0 { println!("  p{p} -> n{j}"); stack.push(j); }
        }
    }
    println!("}}");

    Ok(())
}
