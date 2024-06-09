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
    dir, trim_whitespaces, utils::{count_occourances, four_to_pow, largest_pow_of_four}
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

const BRACE_TY_ROUND: usize = 0;
const BRACE_TY_BRACKET: usize = 1;
const BRACE_TY_CURLY: usize = 2;

impl Word {

    fn matches_braces(raw: &str, value: &str) -> bool {
        // FIXME: recursive frame construction is buggy
        let braces = {
            let mut ctx = BraceCtx { resolved: vec![], stack: vec![] };
            let mut no_brace_start = usize::MAX;
            for (i, chr) in value.chars().enumerate() {
                if chr == '(' {
                    if no_brace_start != usize::MAX {
                        ctx.push_text(no_brace_start, i);
                        no_brace_start = usize::MAX;
                    }
                    ctx.resolved.push(Element::Frame { ty: BRACE_TY_ROUND, stack: vec![], finished: false, });
                    ctx.stack.push(BRACE_TY_ROUND);
                } else if chr == '[' {
                    if no_brace_start != usize::MAX {
                        ctx.push_text(no_brace_start, i);
                        no_brace_start = usize::MAX;
                    }
                    ctx.resolved.push(Element::Frame { ty: BRACE_TY_BRACKET, stack: vec![], finished: false, });
                    ctx.stack.push(BRACE_TY_BRACKET);
                } else if chr == '{' {
                    if no_brace_start != usize::MAX {
                        ctx.push_text(no_brace_start, i);
                        no_brace_start = usize::MAX;
                    }
                    ctx.resolved.push(Element::Frame { ty: BRACE_TY_CURLY, stack: vec![], finished: false, });
                    ctx.stack.push(BRACE_TY_CURLY);
                } else if chr == ')' {
                    if no_brace_start != usize::MAX {
                        ctx.push_text(no_brace_start, i);
                        no_brace_start = usize::MAX;
                    }
                    let val = ctx.stack.pop().unwrap();
                    assert_eq!(val, BRACE_TY_ROUND);
                    ctx.close_brace();
                } else if chr == ']' {
                    if no_brace_start != usize::MAX {
                        ctx.push_text(no_brace_start, i);
                        no_brace_start = usize::MAX;
                    }
                    let val = ctx.stack.pop().unwrap();
                    assert_eq!(val, BRACE_TY_BRACKET);
                    ctx.close_brace();
                } else if chr == '}' {
                    if no_brace_start != usize::MAX {
                        ctx.push_text(no_brace_start, i);
                        no_brace_start = usize::MAX;
                    }
                    let val = ctx.stack.pop().unwrap();
                    assert_eq!(val, BRACE_TY_CURLY);
                    ctx.close_brace();
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
        let min_len = {
            let mut min_len = 0;
            for m in braces.iter()  {
                if let Element::Final { start_idx, end_idx } = m {
                    min_len += *end_idx - *start_idx;
                }
            }
            min_len
        };
        // fail fast if the input isn't even long enough to cover all the required content
        // this also prevents empty inputs from being accepted
        if raw.len() < min_len {
            return false;
        }
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

fn apply_mutations(mutations: &Vec<Element>, val: &str, buf: &mut Vec<String>, pat: &str) {
    println!("muts: {:?}", mutations);
    for m in mutations {
        match m {
            Element::Frame { ty, stack, finished } => {
                if !*finished {
                    panic!("Incomplete parenthesis");
                }
                match *ty {
                    BRACE_TY_ROUND => {
                        apply_mutations(stack, val, buf, pat);
                    },
                    BRACE_TY_BRACKET => {
                        // at least one match
                        let len = buf.len();
                        apply_mutations(stack, val, buf, pat);
                        if buf.len() == len {
                            // no match was found, so fail
                            return;
                        }
                    },
                    BRACE_TY_CURLY => {
                        // at most one match
                        let len = buf.len();
                        apply_mutations(stack, val, buf, pat);
                        println!("got curly {}|{}", buf.len(), len);
                        if len + 1 < buf.len() {
                            // more than one match was found, remove newly added attempts
                            for _ in 0..(buf.len() - len) {
                                buf.pop();
                            }
                            return;
                        }
                    },
                    _ => unreachable!(),
                }
            },
            Element::Final { start_idx, end_idx } => {
                fn apply_final(mutations: &Vec<Element>, val: &str, buf: &mut Vec<String>, pat: &str, start_idx: usize, end_idx: usize) {
                    let chunk_size = end_idx - start_idx;
                    // FIXME: there is an off-by-one error somewhere here on the char cnt
                    if /*val.len() >= chunk_size && */substring(val, 0, chunk_size) == substring(pat, start_idx, chunk_size) {
                        let val = String::from_iter(val.chars().skip(chunk_size));
                        apply_mutations(mutations, &val, buf, pat);
                        println!("pushed val");
                        buf.push(val.clone());
                    }
                }
                apply_final(mutations, val, buf, pat, *start_idx, *end_idx);
                let trimmed = strip_whitespace(*start_idx, *end_idx - *start_idx, pat);
                if trimmed.0 != *start_idx || trimmed.1 != (*end_idx - *start_idx) {
                    let (start_idx, len) = trimmed;
                    let end_idx = start_idx + len;
                    apply_final(mutations, val, buf, pat, start_idx, end_idx);
                }
            },
        }
    }
}

fn strip_whitespace(start_idx: usize, len: usize, val: &str) -> (usize, usize) {
    let mut sub_len = 0;
    let mut add_start = 0;
    let mut iter = val.chars().skip(start_idx);
    loop {
        let c = iter.next();
        if !c.map(|v| v.is_whitespace()).unwrap_or(false) {
            break;
        }
        add_start += 1;
        sub_len += 1;
    }
    let mut iter = val.chars().rev().skip(val.chars().size_hint().0 - (start_idx + len));
    loop {
        if !iter.next().map(|v| v.is_whitespace()).unwrap_or(false) {
            break;
        }
        sub_len += 1;
    }
    let start = start_idx + add_start;
    let new_len = len - sub_len;
    (start, new_len)
}

fn substring(src: &str, off: usize, chars: usize) -> &str {
    let start = size_of_chars(src, 0, off);
    let end = start + size_of_chars(src, off, chars);
    &src[start..end]
}

fn size_of_chars(src: &str, off: usize, chars: usize) -> usize {
    let mut size = 0;
    let mut iter = src.chars().skip(off);
    for _ in 0..chars {
        // FIXME: for some reason this tries one character too much
        if let Some(chr) = iter.next() {
            size += char_size(chr);
        }
    }
    size
}

#[inline]
fn char_size(chr: char) -> usize {
    let chr = chr as u32;
    if chr <= 0x00007F {
        1
    } else if chr <= 0x0007FF {
        2
    } else if chr <= 0x00FFFF {
        3
    } else {
        4
    }
    // ((chr as u32).trailing_ones() as usize).max(1)
}


struct BraceCtx {
    resolved: Vec<Element>,
    stack: Vec<usize>,
}

impl BraceCtx {

    fn push_text(&mut self, start_idx: usize, end_idx: usize) {
        if self.resolved.is_empty() {
            self.resolved.push(Element::Final { start_idx, end_idx });
            return;
        }
        let mut i = 1;
        while i <= self.resolved.len() {
            let idx = self.resolved.len() - i;
            let elem = &mut self.resolved[idx];
            match elem {
                Element::Frame { stack, finished, .. } => {
                    if !*finished {
                        stack.push(Element::Final { start_idx, end_idx });
                        return;
                    }
                },
                Element::Final { .. } => break,
            }
            i += 1;
        }
        self.resolved.push(Element::Final { start_idx, end_idx });
    }

    fn close_brace(&mut self) {
        for i in 1..(self.resolved.len() + 1) {
            let idx = self.resolved.len() - i;
            let elem = &mut self.resolved[idx];
            match elem {
                Element::Frame { finished, .. } => {
                    if !*finished {
                        *finished = true;
                        let brace = self.resolved.pop().unwrap();
                        for elem in self.resolved.iter_mut().rev() {
                            match elem {
                                Element::Frame { stack, finished, .. } => {
                                    if !*finished {
                                        stack.push(brace);
                                        return;
                                    }
                                },
                                Element::Final { .. } => break,
                            }
                        }
                        self.resolved.push(brace);
                        break;
                    }
                },
                Element::Final { .. } => unreachable!(),
            }
        }
    }

}

#[derive(Debug)]
enum Element {
    Frame {
        ty: usize,
        stack: Vec<Element>,
        finished: bool,
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
