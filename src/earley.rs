use std::{iter, mem};
use indexmap::{IndexMap, map::Entry};
use crate::grammar::Grammar;

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

    // Transient data produced for each set.
    // Earley items are augmented with left and right child nodes, one per derivation.
    // They are grouped (and counted) by the eventual parent SPPF node for those children.
    calls: Vec<usize>,
    kinds: IndexMap<(usize, usize), usize>,
    items: Vec<(usize, usize, usize, usize)>,

    // Finalized tree nodes, named either `(!s, start)` or `(slot, start)`.
    // Nodes completed in set `i` are in `nodes[sets[i + 0]..sets[i + 1]]`.
    // Each node `n` has a set of child pairs in `packs[nodes[n + 0]..nodes[n + 1]]`.
    pub sets: Vec<usize>,
    pub nodes: Vec<(usize, usize, usize)>,
    pub packs: Vec<(usize, usize)>,
}

impl<'a> Parse<'a> {
    pub fn from_analysis(analysis: &'a Analysis) -> Parse<'a> {
        let calls = Vec::default();
        let kinds = IndexMap::default();
        let items = Vec::default();
        let sets = Vec::default();
        let nodes = Vec::default();
        let packs = Vec::default();
        let mut parse = Parse { analysis, calls, kinds, items, sets, nodes, packs };

        let Parse { ref mut items, ref mut sets, .. } = parse;
        let &Analysis { terminals, .. } = analysis;

        items.push((terminals, sets.len(), !0, !0));
        parse.epsilon();

        parse
    }

    pub fn parse(&mut self, t: usize) {
        let &mut Parse { ref mut items, ref mut sets, .. } = self;

        // Scan.
        items.push((t, sets.len() - 1, !0, !0));
        self.epsilon();
    }

    fn epsilon(&mut self) {
        let &mut Parse {
            analysis, ref mut calls, ref mut kinds, ref mut items,
            ref mut sets, ref mut nodes, ref mut packs, ..
        } = self;
        let &Analysis { terminals, ref nulls, ref edges, ref rules, ref slots, .. } = analysis;

        let set = sets.len();
        sets.push(nodes.len());

        // Process all items in the current set.
        for item in 0.. {
            let Some(&(slot, start, _, _)) = items.get(item) else { break };
            if (edges[slot] as isize) < 0 {
                // Completions.
                let s = !edges[slot];
                let (j, true) = Self::pack(kinds, (!s, start)) else { continue };
                if start == set { continue; }
                for i in sets[start + 0]..sets[start + 1] {
                    let (slot, start, _) = nodes[i];
                    if (slot as isize) < 0 { continue; }
                    if edges[slot] == s {
                        items.push((slot + 1, start, i, nodes.len() + j));
                    }
                }
            } else {
                // Predictions.
                let s = edges[slot];
                let (i, _) = Self::pack(kinds, (slot, start));
                if s < terminals { continue; }
                if nulls[s] {
                    let (j, _, _) = or_insert_full(kinds.entry((!s, set)), 0);
                    items.push((slot + 1, start, nodes.len() + i, nodes.len() + j));
                }
                if calls.contains(&s) { continue; }
                calls.push(s);
                for &slot in &slots[rules[s + 0]..rules[s + 1]] {
                    items.push((slot, set, !0, !0));
                }
            }
        }

        // Create nodes and allocate indices for their child pairs.
        nodes.reserve(kinds.len());
        let mut sum = packs.len();
        for (&(s, start), count) in kinds.iter_mut() {
            nodes.push((s, start, sum));
            sum += mem::replace(count, sum);
        }

        // Move child pairs from items into their parent nodes.
        let old = packs.len();
        packs.resize(packs.len() + items.len(), (0, 0));
        for &(slot, start, mut i, j) in &items[..] {
            const NULL: usize = !0;

            // TODO: This simplification is at least localized here, but it is still a bit messy.

            // Simplify left children: point to `x` instead of `A -> x . ys`.
            if i != NULL {
                let (islot, istart, ip) = nodes[i];
                if islot < 1 || (edges[islot - 1] as isize) < 0 {
                    i = NULL;
                } else if islot < 2 || (edges[islot - 2] as isize) < 0 {
                    i = if istart < set {
                        let (_, _, iq) = nodes[i + 1];
                        let [(NULL, j)] = packs[ip..iq] else { unreachable!() };
                        j
                    } else {
                        let i = kinds.get_index_of(&(!edges[islot - 1], set)).unwrap();
                        nodes.len() - kinds.len() + i
                    };
                }
            }

            let s = if (edges[slot] as isize) < 0 { edges[slot] } else { slot };
            let p = &mut kinds[&(s, start)];
            packs[mem::replace(p, *p + 1)] = (i, j);
        }
        debug_assert!(!packs[old..].iter().any(|&(i, j)| (i, j) == (0, 0)));

        calls.clear();
        kinds.clear();
        items.clear();
    }

    /// Count an Earley item towards its parent group.
    fn pack(kinds: &mut IndexMap<(usize, usize), usize>, label: (usize, usize)) -> (usize, bool) {
        let (j, inserted, count) = or_insert_full(kinds.entry(label), 0);
        *count += 1;
        (j, inserted)
    }
}

fn or_insert_full<K, V>(entry: Entry<'_, K, V>, default: V) -> (usize, bool, &'_ mut V) {
    match entry {
        Entry::Occupied(entry) => { (entry.index(), false, entry.into_mut()) }
        Entry::Vacant(entry) => { (entry.index(), true, entry.insert(default) ) }
    }
}

#[cfg(test)]
mod tests {
    use super::{Grammar, Analysis, Parse};

    /// Simple "S l o g" grammar: `S -> eps | o | S o | S < S >`.
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
        // Parse the input `o o < o < o > >`.
        let analysis = Analysis::from_grammar(&GRAMMAR);
        let mut parse = Parse::from_analysis(&analysis);
        for &s in &[0, 0, 1, 0, 1, 0, 2, 2] { parse.parse(s); }

        let Parse { analysis, ref sets, ref nodes, ref packs, .. } = parse;
        let &Analysis { terminals, .. } = analysis;

        // The final set should contain an item for a completed `S` rule covering the full input.
        let s = sets[8] + nodes[sets[8]..].iter()
            .position(|&(slot, start, _)| (slot, start) == (!terminals, 0))
            .expect("completed start symbol");

        let [(k, l)] = packs[nodes[s + 0].2..nodes[s + 1].2] else { panic!("ambiguous") };
        let [(j, k)] = packs[nodes[k + 0].2..nodes[k + 1].2] else { panic!("ambiguous") };
        let [(i, j)] = packs[nodes[j + 0].2..nodes[j + 1].2] else { panic!("ambiguous") };

        assert_eq!((nodes[i].0, nodes[i].1), (!3, 0));
        {
            let [(i, j)] = packs[nodes[i + 0].2..nodes[i + 1].2] else { panic!("ambiguous") };

            assert_eq!((nodes[i].0, nodes[i].1), (!3, 0));
            assert_eq!((nodes[j].0, nodes[j].1), (!0, 1));
        }

        assert_eq!((nodes[j].0, nodes[j].1), (!1, 2));

        assert_eq!((nodes[k].0, nodes[k].1), (!3, 3));
        {
            let [(k, l)] = packs[nodes[k + 0].2..nodes[k + 1].2] else { panic!("ambiguous") };
            let [(j, k)] = packs[nodes[k + 0].2..nodes[k + 1].2] else { panic!("ambiguous") };
            let [(i, j)] = packs[nodes[j + 0].2..nodes[j + 1].2] else { panic!("ambiguous") };

            assert_eq!((nodes[i].0, nodes[i].1), (!3, 3));
            assert_eq!((nodes[j].0, nodes[j].1), (!1, 4));
            assert_eq!((nodes[k].0, nodes[k].1), (!3, 5));
            assert_eq!((nodes[l].0, nodes[l].1), (!2, 6));
        }

        assert_eq!((nodes[l].0, nodes[l].1), (!2, 7));
    }
}
