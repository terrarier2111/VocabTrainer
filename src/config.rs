use std::{
    collections::HashMap,
    fmt::{Debug, Display, Write},
    io::Write as IOWrite,
    ops::Deref,
    sync::Mutex,
};

use rand::Rng;
use serde_derive::{Deserialize, Serialize};

use crate::{
    dir,
    utils::{count_occourances, four_to_pow, largest_pow_of_four},
};

#[derive(Serialize, Deserialize, Clone)]
pub struct LearnSetConfig {
    pub questioning: Questioning,
    pub kv_seperator: char,
    pub comment_identifier: char,
    pub ignore_errors: bool,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct Questioning {
    pub given_amount: Amount,
    pub given_direction: Direction,
    pub expected_amount: Amount,
    pub mode: QuestioningMode,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub enum QuestioningMode {
    // possible formats for sets:
    // ID: VAL -> explicit id
    // VAL     -> implicit id
    Order,
    // possible formats for sets of the following modes:
    // VAL, VAL, VAL, ... = VAL, VAL, ...
    KV,
}

#[derive(Serialize, Deserialize, Debug, Clone, Copy, PartialEq)]
pub enum Amount {
    All,
    Any,
}

#[derive(Serialize, Deserialize, Debug, Clone, Copy, PartialEq)]
pub enum Direction {
    Left,
    Right,
    Bi,
}

pub struct Set {
    pub name: String,
    pub kind: SetKind,
    pub config: LearnSetConfig,
    pub meta: Mutex<LearnSetMeta>,
}

impl Set {
    const METRIC_ACCURACY_MAX: f64 = 0.950;
    const METRIC_MOST_RECENT_MAX: f64 = 0.500;
    const METRIC_OVERALL_MAX: f64 = 0.975; // TODO: make these values depend on the size of the set!

    #[inline]
    pub fn size(&self) -> usize {
        match &self.kind {
            SetKind::Order(entries) => entries.len(),
            SetKind::KV(entries) => entries.len(),
        }
    }

    pub fn pick_word(&self, subset: &Vec<usize>) -> Word {
        match &self.kind {
            SetKind::Order(orders) => {
                // TODO: support more questioning modes!
                let idx = if subset.is_empty() {
                    rand::thread_rng().gen_range(0..(orders.len()))
                } else {
                    subset[rand::thread_rng().gen_range(0..(subset.len()))]
                };
                let val = orders.get(idx).unwrap();
                Word::Order {
                    key: (val.0, OrderOrNames::Order(val.0)),
                    value: OrderOrNames::Name((
                        vec![val.1.clone()],
                        self.config.questioning.expected_amount,
                    )),
                }
            }
            SetKind::KV(pairs) => loop {
                let idx = if subset.is_empty() {
                    rand::thread_rng().gen_range(0..(pairs.len()))
                } else {
                    subset[rand::thread_rng().gen_range(0..(subset.len()))]
                };
                let val = &pairs[idx];
                let meta = self.meta.lock().unwrap();
                let entry = meta.entries.kv().unwrap().get(&val.0 .0).unwrap();
                let success_rate = entry.successes as f64 / (entry.tries as f64).max(1.0);
                let avg_success_rate = meta.successes as f64 / (meta.tries as f64).max(1.0);
                let normalized_rate = if success_rate > avg_success_rate {
                    success_rate.sqrt()
                } else {
                    success_rate.powi(2)
                };
                let success_range = normalized_rate * Self::METRIC_ACCURACY_MAX;
                let recency_range = Self::METRIC_MOST_RECENT_MAX
                    / (meta.tries as f64 - entry.last_presented as f64)
                        .max(1.0)
                        .powi(2);
                let skip_range = (success_range + recency_range).min(Self::METRIC_OVERALL_MAX);
                if rand::thread_rng().gen::<f64>() <= skip_range {
                    continue;
                }
                let questioning = &self.config.questioning;
                break match questioning.mode {
                    QuestioningMode::KV => {
                        let left_given = questioning.given_direction == Direction::Left
                            || (questioning.given_direction == Direction::Bi
                                && rand::thread_rng().gen_range(0..=1) == 0);
                        let (key, val) = if left_given {
                            (val.0.clone(), val.1.clone())
                        } else {
                            ((val.0 .0.clone(), val.1.clone()), val.0 .1.clone())
                        };
                        let key = {
                            if questioning.given_amount == Amount::All {
                                key
                            } else {
                                (
                                    key.0,
                                    vec![key.1[rand::thread_rng().gen_range(0..(key.1.len()))]
                                        .clone()],
                                )
                            }
                        };
                        Word::KV {
                            key,
                            value: (val, questioning.expected_amount),
                        }
                    }
                    QuestioningMode::Order => unreachable!(),
                };
            },
        }
    }

    /// safe the config on disk
    pub fn save_cfg(&self) {
        let cfg = dir().join(&format!("{}.json", self.name));
        std::fs::File::create(cfg)
            .unwrap()
            .write_all(serde_json::to_string(&self.config).unwrap().as_bytes())
            .unwrap();
    }

    /// safe the meta data on disk
    pub fn save_meta(&self) {
        let meta = dir().join("cache").join(&format!("{}.json", self.name));
        std::fs::File::create(meta)
            .unwrap()
            .write_all(
                serde_json::to_string(self.meta.lock().unwrap().deref())
                    .unwrap()
                    .as_bytes(),
            )
            .unwrap();
    }
}

#[derive(Debug)]
pub enum OrderOrNames {
    Order(usize),
    Name((Vec<String>, Amount)),
}

pub struct Key<'a>(&'a Word);

impl<'a> Display for Key<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.0 {
            Word::Order { key, .. } => match &key.1 {
                OrderOrNames::Order(order) => f.write_str(order.to_string().as_str()),
                OrderOrNames::Name(name) => {
                    let mut iter = name.0.iter();
                    f.write_str(iter.next().unwrap())?;
                    for key in iter {
                        f.write_str(", ")?;
                        f.write_str(key)?;
                    }
                    Ok(())
                }
            },
            Word::KV { key, .. } => {
                let mut iter = key.1.iter();
                f.write_str(iter.next().unwrap())?;
                for key in iter {
                    f.write_str(", ")?;
                    f.write_str(key)?;
                }
                Ok(())
            }
        }
    }
}

pub struct Value<'a>(&'a Word);

impl<'a> Display for Value<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.0 {
            Word::Order { value, .. } => match value {
                OrderOrNames::Order(order) => f.write_str(order.to_string().as_str()),
                OrderOrNames::Name(names) => {
                    let mut iter = names.0.iter();
                    f.write_str(iter.next().unwrap())?;
                    for val in iter {
                        f.write_str(", ")?;
                        f.write_str(val)?;
                    }
                    Ok(())
                }
            },
            Word::KV { value, .. } => {
                let mut iter = value.0.iter();
                f.write_char('\"')?;
                f.write_str(iter.next().unwrap())?;
                for val in iter {
                    f.write_str("\", \"")?;
                    f.write_str(val)?;
                }
                f.write_char('\"')?;
                Ok(())
            }
        }
    }
}

pub enum Word {
    Order {
        key: (usize, OrderOrNames),
        value: OrderOrNames,
    },
    KV {
        key: (String, Vec<String>),
        value: (Vec<String>, Amount),
    },
}

const BRACE_TY_NO_BRACE: usize = 0;
const BRACE_TY_ROUND: usize = 1;
const BRACE_TY_BRACKET: usize = 2;
const BRACE_TY_CURLY: usize = 3;

impl Word {

    pub fn matches_braces(raw: &str, value: &str) -> bool {
        let braces = {
            let mut ctx = BraceCtx { resolved: vec![], stack: vec![] };
            let mut no_brace_start = usize::MAX;
            // FIXME: insert chhunks at `(` and not at `)`
            for (i, chr) in value.chars().enumerate() {
                if chr == '(' {
                    if no_brace_start != usize::MAX {
                        ctx.push_text(no_brace_start, i);
                        no_brace_start = usize::MAX;
                    }
                    ctx.resolved.push(Element::Frame { ty: BRACE_TY_ROUND, stack: vec![] });
                    ctx.stack.push(BRACE_TY_ROUND);
                } else if chr == '[' {
                    if no_brace_start != usize::MAX {
                        ctx.push_text(no_brace_start, i);
                        no_brace_start = usize::MAX;
                    }
                    ctx.resolved.push(Element::Frame { ty: BRACE_TY_BRACKET, stack: vec![] });
                    ctx.stack.push(BRACE_TY_BRACKET);
                } else if chr == '{' {
                    if no_brace_start != usize::MAX {
                        ctx.push_text(no_brace_start, i);
                        no_brace_start = usize::MAX;
                    }
                    ctx.resolved.push(Element::Frame { ty: BRACE_TY_CURLY, stack: vec![] });
                    ctx.stack.push(BRACE_TY_CURLY);
                } else if chr == ')' {
                    if no_brace_start != usize::MAX {
                        ctx.push_text(no_brace_start, i);
                        no_brace_start = usize::MAX;
                    }
                    let val = ctx.stack.pop().unwrap();
                    assert_eq!(val, BRACE_TY_ROUND);
                } else if chr == ']' {
                    if no_brace_start != usize::MAX {
                        ctx.push_text(no_brace_start, i);
                        no_brace_start = usize::MAX;
                    }
                    let val = ctx.stack.pop().unwrap();
                    assert_eq!(val, BRACE_TY_BRACKET);
                } else if chr == '}' {
                    if no_brace_start != usize::MAX {
                        ctx.push_text(no_brace_start, i);
                        no_brace_start = usize::MAX;
                    }
                    let val = ctx.stack.pop().unwrap();
                    assert_eq!(val, BRACE_TY_CURLY);
                } else if no_brace_start == usize::MAX {
                    no_brace_start = i;
                }
            }
            if no_brace_start != usize::MAX {
                ctx.push_text(no_brace_start, value.len());
            }
            assert!(ctx.stack.is_empty());
            ctx.resolved
        };
        // FIXME: there is a bug in here where when there is a large brace and a smaller brace containd inside the larger brace at its end, it wont detect the thing as working
        // as it will detect that the whole large brace is larger than the provided input because of the braces
        let mut attempts = vec![raw.to_string()];
        while !attempts.is_empty() {
            let val = attempts.remove(0);
            if val.is_empty() {
                return true;
            }
            apply_mutations(&braces, &val, &mut attempts, value);
        }
       false
    }

    pub fn matches(&self, raw: &str) -> bool {
        match self {
            // TODO: support other questioning modes!
            Word::Order { key, value } => raw
                .parse::<usize>()
                .map(|input| input == key.0)
                .unwrap_or(false),
            Word::KV { key, value } => {
                match value.1 {
                    Amount::All => todo!(),
                    Amount::Any => value.0.iter().any(|value| {
                        value.eq_ignore_ascii_case(raw) || {
                            if value.contains('(') && value.contains(')') {
                                // this matching allows "just in time" or "in time" for this given value "(just) in time"
                                Self::matches_braces(raw, value)
                            } else {
                                false
                            }
                        }
                    }),
                }
            }
        }
    }

    pub fn has_multiple_solutions(&self) -> bool {
        match self {
            Word::Order { value, .. } => match value {
                OrderOrNames::Order(_) => false,
                OrderOrNames::Name(value) => value.0.len() > 1 && value.1 == Amount::Any,
            },
            Word::KV { value, .. } => value.0.len() > 1 && value.1 == Amount::Any,
        }
    }

    #[inline]
    pub fn key(&self) -> Key<'_> {
        Key(self)
    }

    #[inline]
    pub fn value(&self) -> Value<'_> {
        Value(self)
    }
}

fn apply_mutations(mutations: &Vec<Element>, val: &String, buf: &mut Vec<String>, pat: &str) {
    println!("muts: {:?}", mutations);
    for m in mutations {
        match m {
            Element::Frame { ty, stack } => {
                match *ty {
                    BRACE_TY_ROUND => {
                        apply_mutations(stack, val, buf, pat);
                    },
                    BRACE_TY_BRACKET => {

                    },
                    BRACE_TY_CURLY => {

                    },
                    _ => unreachable!(),
                }
            },
            Element::Final { start_idx, end_idx } => {
                let chunk_size = *end_idx - *start_idx;
                if val.len() >= chunk_size && cmp_strs(val, 0, pat, *start_idx, chunk_size) {
                    println!("sub match!");
                    let val = String::from_iter(val.chars().skip(chunk_size));
                    apply_mutations(mutations, &val, buf, pat);
                    buf.push(val.clone());
                }
            },
        }
    }
}

struct BraceCtx {
    resolved: Vec<Element>,
    stack: Vec<usize>,
}

impl BraceCtx {

    fn push_text(&mut self, start_idx: usize, end_idx: usize) {
        if let Some(last) = self.resolved.last_mut() {
            match last {
                Element::Frame { ty, stack } => stack.push(Element::Final { start_idx: start_idx, end_idx }),
                Element::Final { .. } => self.resolved.push(Element::Final { start_idx, end_idx }),
            }
        } else {
            self.resolved.push(Element::Final { start_idx, end_idx });
        }
    }

}

fn cmp_strs(first: &str, off1: usize, second: &str, off2: usize, elems: usize) -> bool {
    let mut iter1 = first.chars().skip(off1);
    let mut iter2 = second.chars().skip(off2);
    for _ in 0..elems {
        if iter1.next() != iter2.next() {
            return false;
        }
    }
    true
}

#[derive(Debug)]
enum Element {
    Frame {
        ty: usize,
        stack: Vec<Element>,
    },
    Final {
        start_idx: usize,
        end_idx: usize,
    },
}

#[derive(Clone)]
pub enum SetKind {
    Order(Vec<(usize, String)>),
    KV(Vec<((String, Vec<String>), Vec<String>)>),
}

impl Debug for SetKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Order(_) => f.write_str("Order"),
            Self::KV(_) => f.write_str("KV"),
        }
    }
}

#[derive(Serialize, Deserialize)]
pub struct LearnSetMeta {
    pub successes: usize,
    pub tries: usize,
    pub entries: LearnSetMetaEntries,
}

#[derive(Serialize, Deserialize)]
pub enum LearnSetMetaEntries {
    // the keys are whitespace and lowercase insensitive
    KV(HashMap<String, MetaEntry>),
    // the value here is a combination of a hash and an entry
    Order(HashMap<usize, (usize, MetaEntry)>),
}

impl LearnSetMetaEntries {
    #[inline]
    pub fn kv(&self) -> Option<&HashMap<String, MetaEntry>> {
        match self {
            LearnSetMetaEntries::KV(kv) => Some(kv),
            LearnSetMetaEntries::Order(_) => None,
        }
    }

    #[inline]
    pub fn order(&self) -> Option<&HashMap<usize, (usize, MetaEntry)>> {
        match self {
            LearnSetMetaEntries::KV(_) => None,
            LearnSetMetaEntries::Order(order) => Some(order),
        }
    }

    #[inline]
    pub fn kv_mut(&mut self) -> Option<&mut HashMap<String, MetaEntry>> {
        match self {
            LearnSetMetaEntries::KV(kv) => Some(kv),
            LearnSetMetaEntries::Order(_) => None,
        }
    }

    #[inline]
    pub fn order_mut(&mut self) -> Option<&mut HashMap<usize, (usize, MetaEntry)>> {
        match self {
            LearnSetMetaEntries::KV(_) => None,
            LearnSetMetaEntries::Order(order) => Some(order),
        }
    }

    pub fn get_entry(&self, word: &Word) -> Option<&MetaEntry> {
        match self {
            LearnSetMetaEntries::KV(entries) => match word {
                Word::KV { key, .. } => entries.get(&key.0),
                Word::Order { .. } => unreachable!(),
            },
            LearnSetMetaEntries::Order(entries) => match word {
                Word::Order { key, .. } => entries.get(&key.0).map(|val| &val.1),
                Word::KV { .. } => unreachable!(),
            },
        }
    }

    pub fn get_entry_mut(&mut self, word: &Word) -> Option<&mut MetaEntry> {
        match self {
            LearnSetMetaEntries::KV(entries) => match word {
                Word::KV { key, .. } => entries.get_mut(&key.0),
                Word::Order { .. } => unreachable!(),
            },
            LearnSetMetaEntries::Order(entries) => match word {
                Word::Order { key, .. } => entries.get_mut(&key.0).map(|val| &mut val.1),
                Word::KV { .. } => unreachable!(),
            },
        }
    }
}

#[derive(Serialize, Deserialize)]
pub struct MetaEntry {
    pub successes: usize,
    pub tries: usize,
    pub last_presented: usize,
}

#[derive(PartialEq, Debug)]
enum Restriction {
    None,
    Min,
    Max,
}
