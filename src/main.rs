#![feature(string_remove_matches)]

use std::{io::Write, fs, sync::{Arc, Mutex}, collections::{HashMap, HashSet}, error::Error, fmt::Display, path::PathBuf};

use clitty::{core::{CmdParamNumConstraints, CmdParamStrConstraints, CommandBuilder, CommandImpl, CommandParam, CommandParamTy, UsageBuilder}, ui::{CLIBuilder, CmdLineInterface, FallbackHandler, PrintFallback, Window}};
use config::{LearnSetConfig, Set, MetaEntry, QuestioningMode, Word};
use crossterm::style::Stylize;
use dashmap::DashMap;
use rand::Rng;
use utils::{count_occourances, calculate_hash};

use crate::config::{LearnSetMeta, Questioning, Amount, Direction};

mod utils;
mod config;

pub fn dir() -> PathBuf {
    // dirs::executable_dir().unwrap().join("./VocabTrainer")
    PathBuf::from("./VocabTrainer")
}

fn cache() -> PathBuf {
    let dir = dir();
    dir.join("cache")
}

fn main() -> anyhow::Result<()> {
    let dir = dir();
    let cache = cache();
    std::fs::create_dir_all(&cache).unwrap();
    let mut sets = vec![];
    for set in dir.read_dir().unwrap() {
        let set = set.unwrap();
        if !set.file_type().unwrap().is_file() {
            continue;
        }
        let file_name = set.file_name();
        let split = file_name.to_str().unwrap().rsplit_once('.');
        if let Some((file_name, extension)) = split {
            let config = dir.as_path().join(format!("{}.json", file_name));
            let meta = cache.join(format!("{}.json", file_name));
            if extension.eq_ignore_ascii_case("txt") {
                // check if there is no cfg for this set and create defaults, if necessary
                if !config.exists() {
                    let mut cfg = fs::File::create(&config).unwrap();
                    cfg.write_all(serde_json::to_string(&LearnSetConfig {
                        questioning: Questioning {
                            given_amount: Amount::All,
                            given_direction: Direction::Left,
                            expected_amount: Amount::Any,
                            mode: QuestioningMode::KV,
                        },
                        kv_seperator: '=',
                        comment_identifier: '#',
                        ignore_errors: false,
                    }).unwrap().as_bytes()).unwrap();
                }
                let cfg: LearnSetConfig = serde_json::from_slice(&fs::read(config)?)?;
                let mut meta: LearnSetMeta = if meta.exists() {
                    serde_json::from_slice(&fs::read(meta)?)?
                } else {
                    LearnSetMeta {
                        successes: 0,
                        tries: 0,
                        entries: match cfg.questioning.mode {
                            config::QuestioningMode::Order => config::LearnSetMetaEntries::Order(HashMap::new()),
                            config::QuestioningMode::KV => config::LearnSetMetaEntries::KV(HashMap::new()),
                        },
                    }
                };
                let mut content = fs::read_to_string(dir.join(format!("{}.txt", &file_name)))?;
                content.remove_matches('\r');
                let entries = content.split('\n').filter(|s| !s.starts_with(cfg.comment_identifier)).collect::<Vec<&str>>();
                // ensure all braces are correct
                if entries.iter().enumerate().any(|(i, s)| {
                    let err = {
                        let mut err = false;
                        let mut open = 0;
                        for c in s.chars() {
                            if c == '(' {
                                open += 1;
                            } else if c == ')' {
                                if open == 0 {
                                    err = true;
                                    break;
                                }
                                open -= 1;
                            }
                        }
                        err || open != 0
                    };
                    if err {
                        println!("Set {file_name}: erronous braces in line {i}");
                    }
                    err
                }) {
                    continue;
                }

                match cfg.questioning.mode {
                    config::QuestioningMode::Order => {
                        if count_occourances(&content, cfg.kv_seperator) == entries.len() {
                            let mut err = false;
                            let entries = entries.iter().map(|s| {
                                let split = s.split_once(cfg.kv_seperator);
                                if let Some(split) = split {
                                    if let Ok(num) = split.0.trim().parse::<usize>() {
                                        (num, split.1.trim().to_string())
                                    } else {
                                        err = true;
                                    (0, String::new())
                                    }
                                } else {
                                    err = true;
                                    (0, String::new())
                                }
                            }).collect::<Vec<_>>();
                            if !err {
                                let mut entry_set = HashSet::with_capacity(entries.len());
                                let mut outdated = false;
                                let mut meta = {
                                    for entry in entries.iter() {
                                        entry_set.insert(entry.0);
                                        let entries = meta.entries.order_mut().unwrap();
                                        // ensure metadata is up-to-date
                                        let entry_ref = entries.entry(entry.0);
                                        let hash = calculate_hash(&entry.1) as usize;
                                        let val = entry_ref.or_insert_with(|| {
                                            outdated = true;
                                            (hash, MetaEntry {
                                                successes: 0,
                                                tries: 0,
                                                last_presented: 0,
                                            })
                                        });
                                        if val.0 != hash {
                                            val.0 = hash;
                                            outdated = true;
                                        }
                                    }
                                    meta
                                };
                                let meta_entries = meta.entries.order_mut().unwrap();
                                if meta_entries.len() != entries.len() {                
                                    // cleanup vacant entries            
                                    meta_entries.retain(|key, _| {
                                        let retain = entry_set.contains(key);
                                        outdated |= !retain;
                                        retain
                                    });
                                }
                                let set = Set {
                                    name: file_name.to_string(),
                                    config: cfg.clone(),
                                    meta: Mutex::new(meta),
                                    kind: config::SetKind::Order(entries),
                                };
                                if outdated {
                                    set.save_meta();
                                }
                                sets.push(set);
                                continue;
                            }
                        }
                        sets.push(Set {
                            name: file_name.to_string(),
                            kind: config::SetKind::Order(entries.iter().enumerate().map(|(i, s)| (i, s.trim().to_string())).collect::<Vec<_>>()),
                            config: cfg,
                            meta: Mutex::new(meta),
                        });
                    },
                    config::QuestioningMode::KV => {
                        let mut err = false;
                        let mut outdated = false;
                        let mut entry_set = HashSet::with_capacity(entries.len());
                        let pairs = entries.iter().enumerate().map(|(i, s)| {
                            if let Some((left, right)) = s.split_once(cfg.kv_seperator) {
                                let left = left.trim();
                                let right = right.trim();
                                let left_parts = left.split(',').map(|s| s.trim().to_owned()).collect::<Vec<_>>();
                                let key = trim_whitespaces(&s.to_lowercase());
                                let entries = meta.entries.kv_mut().unwrap();
                                // ensure metadata is up-to-date
                                if !entries.contains_key(&key) {
                                    entries.insert(key.clone(), MetaEntry {
                                        successes: 0,
                                        tries: 0,
                                        last_presented: 0,
                                    });
                                    outdated = true;
                                }
                                entry_set.insert(key.clone());
                                ((key, left_parts), right.split(',').map(|s| s.trim().to_owned()).collect::<Vec<_>>())
                            } else {
                                if !cfg.ignore_errors {
                                    err = true;
                                    println!("Set {file_name}: missing `{}` in line {i}", cfg.kv_seperator);
                                }
                                ((String::new(), vec![]), vec![])
                            }
                        }).filter(|e| !e.0.1.is_empty() && !e.1.is_empty()).collect::<Vec<_>>();
                        if !err {
                            let meta_entries = meta.entries.kv_mut().unwrap();
                            if meta_entries.len() != entries.len() {                            
                                meta_entries.retain(|key, _| {
                                    let retain = entry_set.contains(key);
                                    outdated |= !retain;
                                    retain
                                });
                            }
                            let set = Set {
                                name: file_name.to_string(),
                                kind: config::SetKind::KV(pairs),
                                config: cfg,
                                meta: Mutex::new(meta),
                            };
                            if outdated {
                                set.save_meta();
                            }
                            sets.push(set);
                        }
                    }
                    _ => unreachable!(),
                }
            }
        }
    }
    let set_cnt = sets.len();
    let ctx = Arc::new(TrainingCtx {
        cmd_line: CmdLineInterface::new(Window::new(CLIBuilder::new().prompt("VocabTrainer: ".to_string()).command(
            CommandBuilder::new("help", CmdHelp),
        ).command(
            CommandBuilder::new("sets", CmdSets),
        ).command(
            CommandBuilder::new("learn", CmdLearn).params(UsageBuilder::new().required(CommandParam {
                name: "set",
                ty: CommandParamTy::String(CmdParamStrConstraints::None),
            }))
        ).fallback(Box::new(PrintFallback::new("Please use `help` in order to learn which commands are available".red().to_string()))))),
        sets: {
            let map = DashMap::new();
            for set in sets {
                map.insert(set.name.to_lowercase(), set);
            }
            map
        },
        learn_instance: Mutex::new(None),
    });
    ctx.cmd_line.println(&format!("Found {} sets", set_cnt));
    loop {
        ctx.cmd_line.await_input(&ctx);
    }
}

fn trim_whitespaces(s: &str) -> String {
    let mut result = String::with_capacity(s.len());
    s.split_whitespace().for_each(|w| {
        if !result.is_empty() {
            result.push(' ');
        }
        result.push_str(w);
    });
    result
}

pub struct TrainingCtx {
    cmd_line: CmdLineInterface<Arc<TrainingCtx>>,
    sets: DashMap<String, Set>,
    learn_instance: Mutex<Option<LearnInstance>>,
}

struct LearnInstance {
    set: String,
    curr_word: Word,
    success: usize,
    failed: usize,
    subset_mask: Vec<usize>,
}

struct CmdHelp;

impl CommandImpl for CmdHelp {
    type CTX = Arc<TrainingCtx>;

    fn execute(&self, ctx: &Arc<TrainingCtx>, _input: &[&str]) -> anyhow::Result<()> {
        ctx.cmd_line.println(format!("Commands ({}):", ctx.cmd_line.cmds().size_hint().0).as_str());

        for cmd in ctx.cmd_line.cmds() {
            if let Some(desc) = cmd.desc() {
                ctx.cmd_line.println(format!("{}: {}", cmd.name(), desc).as_str());
            } else {
                ctx.cmd_line.println(format!("{}", cmd.name()).as_str());
            }
        }
        Ok(())
    }
}

struct CmdSets;

impl CommandImpl for CmdSets {
    type CTX = Arc<TrainingCtx>;

    fn execute(&self, ctx: &Self::CTX, _input: &[&str]) -> anyhow::Result<()> {
        for set in ctx.sets.iter() {
            ctx.cmd_line.println(&format!("Found set \"{}\": {:?}", set.value().name, set.value().kind));
        }
        Ok(())
    }
}

#[derive(Debug)]
struct SetDoesNotExistsError(String);

impl Display for SetDoesNotExistsError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("the set ")?;
        f.write_str(&self.0)?;
        f.write_str(" does not exist")
    }
}

impl Error for SetDoesNotExistsError {}

struct CmdLearn;

impl CommandImpl for CmdLearn {
    type CTX = Arc<TrainingCtx>;

    fn execute(&self, ctx: &Arc<TrainingCtx>, input: &[&str]) -> anyhow::Result<()> {
        let set_name = input[0].to_lowercase();
        if let Some(set) = ctx.sets.get(&set_name) {
            let initial = set.pick_word(&vec![]);
                let prompt = format!("{}: ", initial.key());
                *ctx.learn_instance.lock().unwrap() = Some(LearnInstance {
                    set: set_name,
                    curr_word: initial,
                    success: 0,
                    failed: 0,
                    subset_mask: vec![],
                });
                ctx.cmd_line.push_screen(Window::new(CLIBuilder::new().command(
                    CommandBuilder::new("$end", CmdLearnEnd)
                ).command(CommandBuilder::new("$subset", CmdSubset).params(UsageBuilder::new().required(CommandParam {
                    name: "size",
                    ty: CommandParamTy::Int(CmdParamNumConstraints::None),
                }))).fallback(Box::new(InputFallback)).prompt(prompt).on_close(Box::new(|ctx| {
                    let mut instance = ctx.learn_instance.lock().unwrap();
                    let instance = instance.as_mut().unwrap();
                    instance.subset_mask.clear();
                    if instance.success == 0 || instance.failed == 0 {
                        ctx.cmd_line.println(&format!("You learned {}/{} vocabs successfully", instance.success, instance.success + instance.failed));
                    } else {
                        ctx.cmd_line.println(&format!("You learned {}/{} vocabs successfully ({:.2}%)", instance.success, instance.success + instance.failed, 100.0 * (instance.success as f64 / (instance.success as f64 + instance.failed as f64))));
                    }
                }))));
                return Ok(());
        }
        return Err(anyhow::Error::from(SetDoesNotExistsError(set_name)));
    }
}

struct CmdLearnEnd;

impl CommandImpl for CmdLearnEnd {
    type CTX = Arc<TrainingCtx>;

    fn execute(&self, ctx: &Arc<TrainingCtx>, _input: &[&str]) -> anyhow::Result<()> {
        ctx.cmd_line.pop_screen(&ctx);
        Ok(())
    }
}

struct CmdSubset;

impl CommandImpl for CmdSubset {
    type CTX = Arc<TrainingCtx>;

    fn execute(&self, ctx: &Self::CTX, input: &[&str]) -> anyhow::Result<()> {
        let mut learn_instance = ctx.learn_instance.lock().unwrap();
        if let Some(learn_instance) = learn_instance.as_mut() {
            let set_size = ctx.sets.get(&learn_instance.set).unwrap().size();
            let num = input[0].parse::<usize>().unwrap();
            if num >= set_size {
                return Err(anyhow::Error::from(SetLimitError {
                    set: learn_instance.set.clone(),
                    set_size,
                    provided_size: num,
                }));
            }
            learn_instance.subset_mask = {
                let mut subset = vec![];
                let mut remaining = num;
                while remaining != 0 {
                    let gen = rand::thread_rng().gen_range(0..set_size);
                    if !subset.contains(&gen) {
                        subset.push(gen);
                        remaining -= 1;
                    }
                }
                subset
            };
        }
        Ok(())
    }
}

#[derive(Debug)]
struct SetLimitError {
    set: String,
    set_size: usize,
    provided_size: usize,
}

impl Display for SetLimitError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("there is no subset for the set ")?;
        f.write_str(&self.set)?;
        f.write_str(" of size ")?;
        f.write_str(self.provided_size.to_string().as_str())?;
        f.write_str(" as the set itself only has a size of ")?;
        f.write_str(self.set_size.to_string().as_str())
    }
}

impl Error for SetLimitError {}

struct InputFallback;

impl FallbackHandler<Arc<TrainingCtx>> for InputFallback {
    fn handle(&self, input: String, window: &Window<Arc<TrainingCtx>>, ctx: &Arc<TrainingCtx>) -> anyhow::Result<bool> {
        let mut learn_instance = ctx.learn_instance.lock().unwrap();
        if let Some(learn_instance) = learn_instance.as_mut() {
            let set = ctx.sets.get_mut(&learn_instance.set).unwrap();
            {
                let mut meta = set.meta.lock().unwrap();
                meta.tries += 1;
                let tries = meta.tries;
                let entry = meta.entries.get_entry_mut(&learn_instance.curr_word).unwrap();
                entry.tries += 1;
                entry.last_presented = tries;
                if learn_instance.curr_word.matches(input.trim()) {
                    if learn_instance.curr_word.has_multiple_solutions() {
                        window.println(&format!("\"{}\" is correct, among others {}", input, learn_instance.curr_word.value()).green().to_string());
                    } else {
                        window.println(&format!("\"{}\" is correct", input).green().to_string());
                    }
                    learn_instance.success += 1;
                    entry.successes += 1;
                    meta.successes += 1;
                } else {
                    window.println(&format!("\"{}\" is wrong, correct answers are {}", input, learn_instance.curr_word.value()).red().to_string());
                    learn_instance.failed += 1;
                }
            }
            set.save_meta();
            let word = set.pick_word(&learn_instance.subset_mask);
            let prompt = format!("{}: ", word.key());
            window.set_prompt(prompt);
            learn_instance.curr_word = word;
        }
        Ok(true)
    }
}
