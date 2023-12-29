use std::{fmt::{Debug, Display, Write}, collections::HashMap, sync::Mutex, io::Write as IOWrite, ops::Deref};

use rand::Rng;
use serde_derive::{Deserialize, Serialize};

use crate::{utils::count_occourances, dir};

#[derive(Serialize, Deserialize, Clone)]
pub struct LearnSetConfig {
    pub mode: QuestioningMode,
    pub kv_seperator: char,
    pub ignore_errors: bool,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct Questioning {
    pub given_amount: Amount,
    pub given_direction: Direction,
    pub expected_amount: Amount,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub enum QuestioningMode {
    // this is special because it means that the learn set is disabled
    Unconfigured,
    // possible formats for sets:
    // ID: VAL -> explicit id
    // VAL     -> implicit id
    Order,
    // possible formats for sets of the following modes:
    // VAL, VAL, VAL, ... = VAL, VAL, ... 
    KV(Questioning),
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
    pub config: Option<LearnSetConfig>,
    pub meta: Mutex<LearnSetMeta>,
}

impl Set {

    const METRIC_ACCURACY_MAX: f64 = 0.950;
    const METRIC_MOST_RECENT_MAX: f64 = 0.500;
    const METRIC_OVERALL_MAX: f64 = 0.975; // TODO: make these values depend on the size of the set!

    pub fn pick_word(&self) -> ((String, Vec<String>), WordValue) {
        match &self.kind {
            SetKind::Order(orders) => {
                let val = orders.get(rand::thread_rng().gen_range(0..(orders.len()))).unwrap();
                ((val.0.to_string(), vec![val.1.clone()]), WordValue::Order(val.0))
            }
            SetKind::KV(pairs) => {
                loop {
                    let val = pairs.get(rand::thread_rng().gen_range(0..(pairs.len()))).unwrap();
                    let meta = self.meta.lock().unwrap();
                    let entry = meta.entries.get(&val.0.0).unwrap();
                    let success_rate = (entry.successes as f64 / (entry.tries as f64).max(1.0));
                    let avg_success_rate = (meta.successes as f64 / meta.tries as f64);
                    let normalized_rate = if success_rate > avg_success_rate {
                        1.0
                    } else {
                        success_rate
                    }.powi(2);
                    let success_range = normalized_rate * Self::METRIC_ACCURACY_MAX;
                    let recency_range = Self::METRIC_MOST_RECENT_MAX / (meta.tries as f64 - entry.last_presented as f64).max(1.0).powi(2);
                    let skip_range = (success_range + recency_range).min(Self::METRIC_OVERALL_MAX);
                    println!("skip rng: {} success: {} recency: {}", skip_range, success_range, recency_range);
                    if rand::thread_rng().gen::<f64>() <= skip_range {
                        continue;
                    }
                    break match self.config.as_ref().unwrap().mode.clone() {
                        QuestioningMode::KV(questioning) => {
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
                            (key, WordValue::Value(val, questioning.expected_amount))
                        },
                        QuestioningMode::Unconfigured => unreachable!(),
                        QuestioningMode::Order => unreachable!(),
                    };
                }
            },
            SetKind::Unconfigured => unreachable!(),
        }
    }

    /// safe the config on disk
    pub fn save_cfg(&self) {
        let cfg = dir().join(&format!("{}.json", self.name));
        std::fs::File::create(cfg).unwrap().write_all(serde_json::to_string(self.config.as_ref().unwrap()).unwrap().as_bytes());
    }

    /// safe the meta data on disk
    pub fn save_meta(&self) {
        let meta = dir().join("cache").join(&format!("{}.json", self.name));
        std::fs::File::create(meta).unwrap().write_all(serde_json::to_string(self.meta.lock().unwrap().deref()).unwrap().as_bytes());
    }

}

pub enum WordValue {
    Order(usize),
    Value(Vec<String>, Amount),
}

impl WordValue {

    // FIXME: introduce exclusive braces (braces in which only a single subbrace may be present at a time) ´[(te()xt(())eee),(text2)]´
    pub fn matches(&self, raw: &str) -> bool {
        // fun fact: this even supports multiple braces
        fn matches_braces(raw: &str, val: &str) -> bool {
            const STATE_NOOP: u8 = 0;
            const STATE_DEL_BRACE: u8 = 1;
            const STATE_DEL_CONTENT: u8 = 2;
            const STATE_DEL_FOLLOWING_WHITESPACE: u8 = 3;

            let braces = count_occourances(val, '(');
            let mut state_vec = vec![0_u8; braces];
            let mut matches = false;
            for cnt in 0..braces {
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
            WordValue::Order(num) => raw.parse::<usize>().map(|input| input == *num).unwrap_or(false),
            WordValue::Value(val, amount) => {
                match amount {
                    Amount::All => todo!(),
                    Amount::Any => val.iter().any(|val| val.eq_ignore_ascii_case(raw) || {
                        if val.contains('(') && val.contains(')') {
                            // this matching allows "just in time" or "in time" for this given value "(just) in time"
                            matches_braces(raw, val)
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
            WordValue::Order(_) => false,
            WordValue::Value(vals, _) => vals.len() > 1,
        }
    }

}

impl Display for WordValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            WordValue::Order(id) => f.write_str(id.to_string().as_str()),
            WordValue::Value(vals, amount) => {
                let mut iter = vals.iter();
                f.write_char('\"')?;
                f.write_str(iter.next().unwrap())?;
                for val in iter {
                    f.write_str("\", \"")?;
                    f.write_str(val)?;
                }
                f.write_char('\"')?;
                /*f.write_str("\" | ")?;
                f.write_str(match amount {
                    Amount::All => "All",
                    Amount::Any => "Any",
                })?;*/
                Ok(())
            },
        }
    }
}

#[derive(Clone)]
pub enum SetKind {
    Order(Vec<(usize, String)>),
    KV(Vec<((String, Vec<String>), Vec<String>)>),
    Unconfigured,
}

impl Debug for SetKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Order(_) => f.write_str("Order"),
            Self::KV(_) => f.write_str("KV"),
            Self::Unconfigured => f.write_str("Unconfigured"),
        }
    }
}

#[derive(Serialize, Deserialize)]
pub struct LearnSetMeta {
    pub successes: usize,
    pub tries: usize,
    // the keys are whitespace and lowercase insensitive
    pub entries: HashMap<String, MetaEntry>,
}

#[derive(Serialize, Deserialize)]
pub struct MetaEntry {
    pub successes: usize,
    pub tries: usize,
    pub last_presented: usize,
}
