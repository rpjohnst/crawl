use std::{iter, mem};
use std::collections::HashSet;

/// A context free grammar. Rules `r` is stored as `lefts[r] -> words[rules[r + 0]..rules[r + 1]]`.
pub struct Grammar<'g> {
    pub terminals: usize,
    pub variables: usize,
    pub lefts: &'g [usize],
    pub rules: &'g [usize],
    pub words: &'g [usize],
}

/// A compiled form of a context free grammar, for use by an Earley parser.
///
/// Each rule `A -> xs...` is flattened into `edges` as `[xs..., !A]`. An LR(0) item such as
/// `A -> xs . ys` (also a "slot" or "dotted rule") is an index into `edges`.
///
/// The starting items for each nonterminal `s` are stored in `slots[rules[s + 0]..rules[s + 1]]`.
pub struct Analysis {
    terminals: usize,
    nulls: Vec<bool>,
    edges: Vec<usize>,
    rules: Vec<usize>,
    slots: Vec<usize>,
}

impl Analysis {
    pub fn from_grammar(grammar: &Grammar<'_>) -> Analysis {
        let &Grammar { terminals, variables, .. } = grammar;
        let syms = terminals + variables;

        // The containing rules for each occurrence of a symbol `s`,
        // stored in `rules[uses[s + 0]..uses[s + 1]]`.
        let mut uses = Vec::from_iter(iter::repeat(0).take(1 + syms));
        for &s in &grammar.words[..] { uses[1 + s] += 1; }
        let mut sum = 0;
        for n in &mut uses[1..] { sum += mem::replace(n, sum); }
        let mut rules = Vec::from_iter(iter::repeat(0).take(grammar.words.len()));
        for (rule, ij) in grammar.rules.windows(2).enumerate() {
            for &s in &grammar.words[ij[0]..ij[1]] {
                rules[uses[1 + s]] = rule;
                uses[1 + s] += 1;
            }
        }

        // Whether each symbol is nullable.
        let mut nulls = Vec::from_iter(iter::repeat(false).take(syms));
        let mut deps = Vec::from_iter(iter::repeat(0).take(grammar.lefts.len()));
        let mut stack = Vec::default();
        for (rule, ij) in grammar.rules.windows(2).enumerate() {
            deps[rule] = ij[1] - ij[0];
            if ij[0] < ij[1] { continue; }
            let left = grammar.lefts[rule];
            if !mem::replace(&mut nulls[left], true) { stack.push(left); }
        }
        while let Some(left) = stack.pop() {
            for &rule in &rules[uses[left + 0]..uses[left + 1]] {
                deps[rule] -= 1;
                if deps[rule] != 0 { continue; }
                let left = grammar.lefts[rule];
                if !mem::replace(&mut nulls[left], true) { stack.push(left); }
            }
        }

        // Edges of the grammar's PDA, as described in `Analysis`.
        // Generate an extra rule `T -> ` for each terminal, for use by Earley scanning.
        // Also generate an extra "augmented" start rule `S' -> S` for the start symbol.
        let mut edges = Vec::default();
        for t in 0..terminals { edges.push(!t); }
        edges.extend_from_slice(&[terminals, !syms]);
        let mut calls = Vec::default();
        for (rule, ij) in grammar.rules.windows(2).enumerate() {
            calls.push((grammar.lefts[rule], edges.len()));
            edges.extend_from_slice(&grammar.words[ij[0]..ij[1]]);
            edges.push(!grammar.lefts[rule]);
        }
        let mut rules = Vec::from_iter(iter::repeat(0).take(1 + syms));
        for &(left, _) in &calls[..] { rules[1 + left] += 1; }
        let mut sum = 0;
        for n in &mut rules[1..] { sum += mem::replace(n, sum); }
        let mut slots = Vec::from_iter(iter::repeat(0).take(sum));
        for &(left, slot) in &calls[..] {
            slots[rules[1 + left]] = slot;
            rules[1 + left] += 1;
        }

        Analysis { terminals, nulls, edges, rules, slots }
    }
}

pub struct Parse<'a> {
    analysis: &'a Analysis,

    sets: Vec<usize>,
    items: Vec<(usize, usize)>,

    hash: HashSet<(usize, usize)>,
}

impl<'a> Parse<'a> {
    pub fn from_analysis(analysis: &'a Analysis) -> Parse<'a> {
        let sets = Vec::default();
        let items = Vec::default();
        let hash = HashSet::default();
        let mut parse = Parse { analysis, sets, items, hash };

        let Parse { ref mut items, ref mut sets, .. } = parse;
        let &Analysis { terminals, .. } = analysis;

        let set = sets.len();
        sets.push(items.len());
        items.push((terminals, set));
        parse.epsilon();

        parse
    }

    pub fn parse(&mut self, t: usize) {
        let &mut Parse { ref mut items, ref mut sets, .. } = self;

        // Scan.
        let set = sets.len();
        sets.push(items.len());
        items.push((t, set - 1));
        self.epsilon();
    }

    fn epsilon(&mut self) {
        let &mut Parse { analysis, ref mut sets, ref mut items, ref mut hash, .. } = self;
        let &Analysis { terminals, ref nulls, ref edges, ref rules, ref slots, .. } = analysis;

        let set = sets.len() - 1;
        for item in sets[set].. {
            let Some(&(slot, start)) = items.get(item) else { break };
            if (edges[slot] as isize) < 0 {
                // Completions.
                let s = !edges[slot];
                if !hash.insert((!s, start)) { continue; }
                if start == set { continue; }
                for i in sets[start + 0]..sets[start + 1] {
                    let (slot, start) = items[i];
                    if (slot as isize) < 0 { continue; }
                    if edges[slot] == s { items.push((slot + 1, start)); }
                }
            } else {
                // Predictions.
                let s = edges[slot];
                if s < terminals { continue; }
                if nulls[s] { items.push((slot + 1, start)); }
                if !hash.insert((s, 0)) { continue; }
                for &slot in &slots[rules[s + 0]..rules[s + 1]] { items.push((slot, set)); }
            }
        }

        hash.clear();
    }
}

#[cfg(test)]
mod tests {
    use super::{Grammar, Analysis, Parse};

    /// Simple "S l o g" grammar: `S -> eps | o | S o | S < o >`.
    static GRAMMAR: Grammar<'_> = Grammar {
        terminals: 3,
        variables: 1,
        lefts: &[3, 3, 3, 3],
        rules: &[0, 0, 1, 3, 7],
        words: &[
            0,
            3, 0,
            3, 1, 3, 2,
        ],
    };

    #[test]
    fn slog() {
        // Recognize the input `o o < o < o > >`.
        let analysis = Analysis::from_grammar(&GRAMMAR);
        let mut parse = Parse::from_analysis(&analysis);
        for &s in &[0, 0, 1, 0, 1, 0, 2, 2] { parse.parse(s); }

        let Parse { analysis, ref sets, ref items, .. } = parse;
        let &Analysis { terminals, .. } = analysis;

        // The final set should contain an item for a completed `S` rule covering the full input.
        items[sets[8]..].iter()
            .position(|&(slot, start)| (slot, start) == (terminals + 1, 0))
            .expect("completed start symbol");
    }
}
