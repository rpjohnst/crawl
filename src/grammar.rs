use crate::symbols::{SymbolMap, Symbol};
use indexmap::IndexSet;

/// A context free grammar. Rule `r` is stored as `lefts[r] -> words[rules[r + 0]..rules[r + 1]]`.
pub struct Grammar<'g> {
    pub terminals: usize,
    pub variables: usize,
    pub lefts: &'g [usize],
    pub rules: &'g [usize],
    pub words: &'g [usize],
}

#[derive(Default)]
pub struct GrammarBuf {
    lefts: Vec<usize>,
    rules: Vec<usize>,
    words: Vec<usize>,
}

/// Grammar DSL.
///
/// An implementation defines rules with `f.def("A").alt(("x", "y".opt(), "z", ...)).alt(...)`.
pub trait Rules {
    fn walk_rules(&self, f: &mut impl VisitRules);
    fn walk_terminals(&self, f: &mut impl VisitTerminals);
}
pub trait VisitRules {
    fn visit_rule(&mut self, left: impl Term, word: impl Word);
    fn def<T>(&mut self, left: T) -> Def<'_, Self, T> where Self: Sized { Def(self, left) }
}
pub trait VisitTerminals {
    fn visit_terminal(&mut self, term: &[u8]);
    fn token(&mut self, term: impl Term) -> &mut Self {
        term.walk(|term, optional| {
            assert!(!optional, "terminals must not be optional");
            self.visit_terminal(term);
        });
        self
    }
}

pub trait Word { fn walk(&self, f: &mut impl VisitWord); }
pub trait VisitWord { fn visit_term(&mut self, term: impl Term); }

pub trait Term {
    fn walk(&self, f: impl FnOnce(&[u8], bool));
    fn opt(self) -> Opt<Self> where Self: Sized { Opt(self) }
}

pub struct Def<'a, V, T>(&'a mut V, T);

impl<V: VisitRules, T: Term + Clone> Def<'_, V, T> {
    pub fn alt(&mut self, word: impl Word) -> &mut Self {
        let Def(visit, left) = self;
        visit.visit_rule(left.clone(), word);
        self
    }
}

macro_rules! impl_word { (($($t:ident),*)) => {
    impl<$($t),*> Word for ($($t,)*) where
        $($t: Term + Copy),*
    {
        #[allow(unused, nonstandard_style)]
        fn walk(&self, f: &mut impl VisitWord) {
            let &($($t,)*) = self;
            $(f.visit_term($t);)*
        }
    }
} }

impl_word!(());
impl_word!((T0));
impl_word!((T0, T1));
impl_word!((T0, T1, T2));
impl_word!((T0, T1, T2, T3));
impl_word!((T0, T1, T2, T3, T4));
impl_word!((T0, T1, T2, T3, T4, T5));
impl_word!((T0, T1, T2, T3, T4, T5, T6));
impl_word!((T0, T1, T2, T3, T4, T5, T6, T7));
impl_word!((T0, T1, T2, T3, T4, T5, T6, T7, T8));

impl<T> Word for T where T: Term + Clone {
    fn walk(&self, f: &mut impl VisitWord) { f.visit_term(self.clone()); }
}

impl Term for &'_ str {
    fn walk(&self, f: impl FnOnce(&[u8], bool)) { Term::walk(&self.as_bytes(), f); }
}
impl Term for &'_ [u8] {
    fn walk(&self, f: impl FnOnce(&[u8], bool)) { f(self, false); }
}

#[repr(transparent)]
#[derive(Copy, Clone)]
pub struct Opt<T>(T);

impl<'a, T: Term> Term for Opt<T> {
    fn walk(&self, f: impl FnOnce(&[u8], bool)) {
        let Opt(term) = self;
        term.walk(move |term, _| f(term, true));
    }
}

impl<'g> Grammar<'g> {
    pub fn build<'i>(rules: &impl Rules, symbols: &'i SymbolMap<'i, ()>, buf: &'g mut GrammarBuf) ->
        (Read<'i>, Grammar<'g>)
    {
        // First pass: collect symbol names and assign indices.
        let terminals = IndexSet::new();
        let variables = IndexSet::new();
        let optionals = IndexSet::new();
        let mut read = Read { symbols, terminals, variables, optionals };
        rules.walk_terminals(&mut read);
        rules.walk_rules(&mut read);

        // Second pass: resolve names to indices and flatten rules into `GrammarBuf`.
        buf.lefts.clear();
        buf.rules.clear();
        buf.words.clear();
        let mut eval = Eval { read, buf };
        rules.walk_rules(&mut eval);

        // Create rules for optionals.
        for optional in 0..eval.read.optionals.len() {
            let left = eval.read.terminals.len() + eval.read.variables.len() + optional;
            let term = eval.lookup(eval.read.optionals[optional]);

            eval.buf.lefts.push(left);
            eval.buf.rules.push(eval.buf.words.len());

            eval.buf.lefts.push(left);
            eval.buf.rules.push(eval.buf.words.len());
            eval.buf.words.push(term);
        }
        eval.buf.rules.push(eval.buf.words.len());

        let Eval { read, buf } = eval;
        let terminals = read.terminals.len();
        let variables = read.variables.len() + read.optionals.len();
        let GrammarBuf { lefts, rules, words } = buf;
        let grammar = Grammar { terminals, variables, lefts, rules, words };
        (read, grammar)
    }
}

pub struct Read<'i> {
    symbols: &'i SymbolMap<'i, ()>,
    pub terminals: IndexSet<Symbol<'i, ()>>,
    pub variables: IndexSet<Symbol<'i, ()>>,
    pub optionals: IndexSet<Symbol<'i, ()>>,
}

impl VisitTerminals for Read<'_> {
    fn visit_terminal(&mut self, term: &[u8]) {
        let term = self.symbols.intern(term, ());
        self.terminals.insert(term);
    }
}

impl VisitRules for Read<'_> {
    fn visit_rule(&mut self, left: impl Term, word: impl Word) {
        left.walk(|left, _| {
            let left = self.symbols.intern(left, ());
            self.variables.insert(left);
            assert!(!self.terminals.contains(&left), "terminals and variables must be disjoint");
        });
        word.walk(self);
    }
}

impl VisitWord for Read<'_> {
    fn visit_term(&mut self, term: impl Term) {
        term.walk(move |term, optional| {
            let term = self.symbols.intern(term, ());
            if optional { self.optionals.insert(term); }
        });
    }
}

struct Eval<'i, 'g> {
    read: Read<'i>,
    buf: &'g mut GrammarBuf,
}

impl VisitRules for Eval<'_, '_> {
    fn visit_rule(&mut self, left: impl Term, word: impl Word) {
        let read = &self.read;
        left.walk(|left, _| {
            let left = read.symbols.intern(left, ());
            let left = read.variables.get_index_of(&left).unwrap();
            self.buf.lefts.push(read.terminals.len() + left);
        });
        self.buf.rules.push(self.buf.words.len());
        word.walk(self);
    }
}

impl VisitWord for Eval<'_, '_> {
    fn visit_term(&mut self, term: impl Term) {
        term.walk(move |term, optional| {
            let read = &mut self.read;
            let term = read.symbols.intern(term, ());
            let term = if optional {
                let term = read.optionals.get_index_of(&term).unwrap();
                read.symbols.len() + term
            } else {
                self.lookup(term)
            };
            self.buf.words.push(term);
        });
    }
}

impl<'i> Eval<'i, '_> {
    fn lookup(&mut self, term: Symbol<'i, ()>) -> usize {
        let read = &mut self.read;
        if let Some(term) = read.terminals.get_index_of(&term) {
            term
        } else {
            let (term, _) = read.variables.insert_full(term);
            read.terminals.len() + term
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::symbols::SymbolMap;
    use super::{Grammar, GrammarBuf, Rules, VisitRules, VisitTerminals, Term};

    #[test]
    fn slog() {
        struct Slog;

        impl Rules for Slog {
            fn walk_rules(&self, f: &mut impl VisitRules) {
                f.def("S")
                    .alt(())
                    .alt("o")
                    .alt(("S", "o"))
                    .alt(("S", "<", "S", ">".opt()))
                ;
            }

            fn walk_terminals(&self, f: &mut impl VisitTerminals) {
                f
                    .token("o")
                    .token("<")
                    .token(">")
                ;
            }
        }

        let symbols = &SymbolMap::default();
        let mut buf = GrammarBuf::default();
        let (read, grammar) = Grammar::build(&Slog, symbols, &mut buf);

        assert!(read.terminals.iter().map(|t| t.key()).eq([b"o", b"<", b">"]));
        assert!(read.variables.iter().map(|t| t.key()).eq([b"S"]));
        assert!(read.optionals.iter().map(|t| t.key()).eq([b">"]));

        assert_eq!(grammar.terminals, 3);
        assert_eq!(grammar.variables, 2);
        assert_eq!(grammar.lefts, &[3, 3, 3, 3, 4, 4]);
        assert_eq!(grammar.rules, &[0, 0, 1, 3, 7, 7, 8]);
        assert_eq!(grammar.words, &[0, 3, 0, 3, 1, 3, 4, 2]);
    }
}
