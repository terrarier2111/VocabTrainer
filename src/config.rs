use std::{fmt::{Debug, Display}, collections::HashMap, sync::Mutex, io::Write as IOWrite, ops::Deref};

use rand::Rng;
use serde_derive::{Deserialize, Serialize};

use crate::{utils::{count_occourances, four_to_pow, largest_pow_of_four}, dir};

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
                    value: OrderOrNames::Name((vec![val.1.clone()], self.config.questioning.expected_amount)),
                }
            }
            SetKind::KV(pairs) => {
                loop {
                    let idx = if subset.is_empty() {
                        rand::thread_rng().gen_range(0..(pairs.len()))
                    } else {
                        subset[rand::thread_rng().gen_range(0..(subset.len()))]
                    };
                    let val = &pairs[idx];
                    let meta = self.meta.lock().unwrap();
                    let entry = meta.entries.kv().unwrap().get(&val.0.0).unwrap();
                    let success_rate = entry.successes as f64 / (entry.tries as f64).max(1.0);
                    let avg_success_rate = meta.successes as f64 / (meta.tries as f64).max(1.0);
                    let normalized_rate = if success_rate > avg_success_rate {
                        success_rate.sqrt()
                    } else {
                        success_rate.powi(2)
                    };
                    let success_range = normalized_rate * Self::METRIC_ACCURACY_MAX;
                    let recency_range = Self::METRIC_MOST_RECENT_MAX / (meta.tries as f64 - entry.last_presented as f64).max(1.0).powi(2);
                    let skip_range = (success_range + recency_range).min(Self::METRIC_OVERALL_MAX);
                    if rand::thread_rng().gen::<f64>() <= skip_range {
                        continue;
                    }
                    let questioning = &self.config.questioning;
                    break match questioning.mode {
                        QuestioningMode::KV => {
                            let left_given = questioning.given_direction == Direction::Left || (questioning.given_direction == Direction::Bi && rand::thread_rng().gen_range(0..=1) == 0);
                            let (key, val) = if left_given {
                                (val.0.clone(), val.1.clone())
                            } else {
                                ((val.0.0.clone(), val.1.clone()), val.0.1.clone())
                            };
                            let key = {
                                if questioning.given_amount == Amount::All {
                                    key
                                } else {
                                    (key.0, vec![key.1[rand::thread_rng().gen_range(0..(key.1.len()))].clone()])
                                }
                            };
                            Word::KV { key, value: (val, questioning.expected_amount) }
                        },
                        QuestioningMode::Order => unreachable!(),
                    };
                }
            },
        }
    }

    /// safe the config on disk
    pub fn save_cfg(&self) {
        let cfg = dir().join(&format!("{}.json", self.name));
        std::fs::File::create(cfg).unwrap().write_all(serde_json::to_string(&self.config).unwrap().as_bytes()).unwrap();
    }

    /// safe the meta data on disk
    pub fn save_meta(&self) {
        let meta = dir().join("cache").join(&format!("{}.json", self.name));
        std::fs::File::create(meta).unwrap().write_all(serde_json::to_string(self.meta.lock().unwrap().deref()).unwrap().as_bytes()).unwrap();
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
            Word::Order { key, .. } => {
                match &key.1 {
                    OrderOrNames::Order(order) => f.write_str(order.to_string().as_str()),
                    OrderOrNames::Name(name) => {
                        let mut iter = name.0.iter();
                        f.write_str(iter.next().unwrap())?;
                        for key in iter {
                            f.write_str(", ")?;
                            f.write_str(key)?;
                        }
                        Ok(())
                    },
                }
            }
            Word::KV { key, .. } => {
                let mut iter = key.1.iter();
                f.write_str(iter.next().unwrap())?;
                for key in iter {
                    f.write_str(", ")?;
                    f.write_str(key)?;
                }
                Ok(())
            },
        }
    }
}

pub struct Value<'a>(&'a Word);

impl<'a> Display for Value<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.0 {
            Word::Order { value, .. } => {
                match value {
                    OrderOrNames::Order(order) => f.write_str(order.to_string().as_str()),
                    OrderOrNames::Name(names) => {
                        let mut iter = names.0.iter();
                        f.write_str(iter.next().unwrap())?;
                        for val in iter {
                            f.write_str(", ")?;
                            f.write_str(val)?;
                        }
                        Ok(())
                    },
                }
            }
            Word::KV { value, .. } => {
                let mut iter = value.0.iter();
                f.write_str(iter.next().unwrap())?;
                for val in iter {
                    f.write_str(", ")?;
                    f.write_str(val)?;
                }
                Ok(())
            },
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

impl Word {

    // FIXME: introduce exclusive braces (braces in which only a single subbrace may be present at a time) ´[(te()xt(())eee),(text2)]´
    pub fn matches(&self, raw: &str) -> bool {
        // fun fact: this even supports multiple braces
        fn matches_braces(raw: &str, val: &str) -> bool {
            const STATE_NOOP: u8 = 0;
            const STATE_DEL_BRACE: u8 = 1;
            const STATE_DEL_CONTENT: u8 = 2;
            const STATE_DEL_FOLLOWING_WHITESPACE: u8 = 3;

            const STATE_CNT: usize = 4;

            let braces = count_occourances(val, '(');
            // fast path
            if braces == 0 {
                return false;
            }
            let mut state_vec = vec![0_u8; braces];
            let mut matches = false;

            // ensure we have exactly 4 states
            assert_eq!(STATE_CNT, 4);

            for cnt in 0..(four_to_pow(braces)) {
                let cnt = largest_pow_of_four(cnt);
                let val_chars = val.chars();
                let mut raw_chars = raw.chars();
                let mut open = vec![];
                let mut last_skipped = false;
                let mut brace_idx = 0;
                let mut open_cnt = 0;
                let mut skip_content = 0;

                // simulate an increment over the (bit)state vector
                for b in 0..state_vec.len() {
                    if state_vec[b] == STATE_DEL_FOLLOWING_WHITESPACE {
                        state_vec[b] = STATE_NOOP;
                        continue;
                    }
                    state_vec[b] += 1;
                    break;
                }
                for chr in val_chars {
                    if chr == '(' {
                        let state = state_vec[brace_idx];
                        open.push(state);
                        if state == STATE_DEL_CONTENT {
                            skip_content += 1;
                        }
                        brace_idx += 1;
                        if state == STATE_DEL_BRACE || state == STATE_DEL_CONTENT {
                            continue;
                        }
                    }
                    if chr == ')' {
                        let state = open.pop().unwrap();
                        if state == STATE_DEL_CONTENT {
                            skip_content -= 1;
                        }
                        if state == STATE_DEL_FOLLOWING_WHITESPACE {
                            last_skipped = true;
                        }
                        if state == STATE_DEL_BRACE || state == STATE_DEL_CONTENT {
                            continue;
                        }
                    }
                    if skip_content != 0 {
                        last_skipped = true;
                        continue;
                    }
                    if chr.is_whitespace() && last_skipped {
                        continue;
                    }
                    last_skipped = false;
                    matches |= raw_chars.next().map(|r_chr| chr.eq_ignore_ascii_case(&r_chr)).unwrap_or(false);
                }
            }
            matches
        }

        match self {
            // TODO: support other questioning modes!
            Word::Order { key, value } => raw.parse::<usize>().map(|input| input == key.0).unwrap_or(false),
            Word::KV { key, value } => {
                match value.1 {
                    Amount::All => todo!(),
                    Amount::Any => value.0.iter().any(|value| value.eq_ignore_ascii_case(raw) || {
                        if value.contains('(') && value.contains(')') {
                            // this matching allows "just in time" or "in time" for this given value "(just) in time"
                            matches_braces(raw, value)
                        } else {
                            false
                        }
                    }),
                }
            },
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
