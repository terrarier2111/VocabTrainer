#![feature(string_remove_matches)]

use std::{io::Write, fs, sync::{Arc, Mutex}, collections::HashMap, ops::Deref, error::Error, fmt::Display, path::PathBuf};

use cli_core::CommandImpl;
use cmd_line::{Window, CmdLineInterface, FallbackHandler};
use config::{LearnSetConfig, Set, WordValue, MetaEntry};
use crossterm::style::Stylize;
use dashmap::DashMap;
use utils::{input, count_occourances};

use crate::{cmd_line::{CLIBuilder, PrintFallback}, config::{LearnSetMeta, Questioning, Amount, Direction}, cli_core::{CommandBuilder, CommandParamTy, CmdParamStrConstraints, UsageBuilder, CommandParam}};

mod utils;
mod config;
mod cmd_line;
mod cli_core;

pub fn dir() -> PathBuf {
    // dirs::executable_dir().unwrap().join("./VocabTrainer")
    PathBuf::from("./VocabTrainer")
}

fn main() -> anyhow::Result<()> {
    let dir = dir();
    let cache = dir.join("cache");
    std::fs::create_dir_all(&cache);
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
                // check if there is no cfg for this set
                if !config.exists() {
                    sets.push(Set {
                        name: file_name.to_string(),
                        kind: config::SetKind::Unconfigured,
                        config: None,
                        meta: Mutex::new(LearnSetMeta { successes: 0, tries: 0, entries: HashMap::new() }),
                    });
                    continue;
                }
                let mut cfg: LearnSetConfig = serde_json::from_slice(&fs::read(config)?)?;
                let mut meta: LearnSetMeta = if meta.exists() {
                    serde_json::from_slice(&fs::read(meta)?)?
                } else {
                    LearnSetMeta {
                        successes: 0,
                        tries: 0,
                        entries: HashMap::new(),
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

                match cfg.mode {
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
                                let mut outdated = false;
                                let set = Set {
                                    name: file_name.to_string(),
                                    config: Some(cfg.clone()),
                                    meta: {
                                        for entry in entries.iter() {
                                            let key = entry.0.to_string();
                                            // ensure metadata is up-to-date
                                            if !meta.entries.contains_key(&key) {
                                                outdated = true;
                                                meta.entries.insert(key, MetaEntry {
                                                    successes: 0,
                                                    tries: 0,
                                                    last_presented: 0,
                                                });
                                            }
                                        }
                                        Mutex::new(meta)
                                    },
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
                            config: Some(cfg),
                            meta: todo!(),
                        });
                    },
                    config::QuestioningMode::KV(ref questioning) => {
                        let mut err = false;
                        let mut outdated = false;
                        let pairs = entries.iter().enumerate().map(|(i, s)| {
                            if let Some((left, right)) = s.split_once(cfg.kv_seperator) {
                                let left = left.trim();
                                let right = right.trim();
                                let left_parts = left.split(',').map(|s| s.trim().to_owned()).collect::<Vec<_>>();
                                let mut key = trim_whitespaces(&s.to_lowercase());
                                // ensure metadata is up-to-date
                                if !meta.entries.contains_key(&key) {
                                    meta.entries.insert(key.clone(), MetaEntry {
                                        successes: 0,
                                        tries: 0,
                                        last_presented: 0,
                                    });
                                    outdated = true;
                                }
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
                            let set = Set {
                                name: file_name.to_string(),
                                kind: config::SetKind::KV(pairs),
                                config: Some(cfg),
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
                name: "set".to_string(),
                ty: CommandParamTy::String(CmdParamStrConstraints::None),
            }))
        ).command(
            CommandBuilder::new("setup", CmdSetup).params(UsageBuilder::new().required(CommandParam {
                name: "set".to_string(),
                ty: CommandParamTy::String(CmdParamStrConstraints::None),
            }))
        ).fallback(Box::new(PrintFallback("Please use `help` in order to learn which commands are available".red().to_string()))))),
        sets: {
            let mut map = DashMap::new();
            for set in sets {
                map.insert(set.name.to_lowercase(), set);
            }
            map
        },
        learn_instance: Mutex::new(None),
    });
    ctx.cmd_line.println(&format!("Found {} sets", set_cnt)).unwrap();
    // FIXME: add exit command!
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
    cmd_line: CmdLineInterface,
    sets: DashMap<String, Set>,
    learn_instance: Mutex<Option<LearnInstance>>,
}

struct LearnInstance {
    set: String,
    curr_word: (String, Vec<String>),
    curr_val: WordValue,
    success: usize,
    failed: usize,
}

struct CmdHelp;

impl CommandImpl for CmdHelp {
    type CTX = Arc<TrainingCtx>;

    fn execute(&self, ctx: &Arc<TrainingCtx>, _input: &[&str]) -> anyhow::Result<()> {
        ctx.cmd_line.println(format!("Commands ({}):", ctx.cmd_line.cmds().len()).as_str());

        for cmd in ctx.cmd_line.cmds().deref() {
            if let Some(desc) = cmd.1.desc() {
                ctx.cmd_line.println(format!("{}: {}", cmd.1.name(), desc).as_str());
            } else {
                ctx.cmd_line.println(format!("{}", cmd.1.name()).as_str());
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
            ctx.cmd_line.println(&format!("Found {}: {:?}", set.value().name, set.value().kind)).unwrap();
        }
        Ok(())
    }
}

struct CmdSetup;

impl CommandImpl for CmdSetup {
    type CTX = Arc<TrainingCtx>;

    fn execute(&self, ctx: &Arc<TrainingCtx>, input: &[&str]) -> anyhow::Result<()> {
        let set_name = input[0].to_lowercase();
        let dir = dir();
        let file_path = dir.join(format!("{}.txt", &set_name));
        if !file_path.exists() {
            println!("file {:?}", dir.join(format!("{}.txt", &set_name)));
            return Err(anyhow::Error::from(SetDoesNotExistsError(set_name)));
        }
        let cfg = dir.join(format!("{}.json", &set_name));
        let mut cfg = fs::File::create(cfg).unwrap();
        cfg.write_all(serde_json::to_string(&LearnSetConfig {
            mode: config::QuestioningMode::KV(Questioning {
                given_amount: Amount::All,
                given_direction: Direction::Left,
                expected_amount: Amount::Any,
            }),
            kv_seperator: '=',
            comment_identifier: '#',
            ignore_errors: false,
        }).unwrap().as_bytes()).unwrap();
        ctx.cmd_line.println("Created default config");
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
            if set.config.is_some() {
                let initial = set.pick_word();
                let prompt = {
                    let mut prompt = String::new();
                    for key in initial.0.1.iter() {
                        prompt.push_str(key);
                        prompt.push_str(", ");
                    }
                    prompt.pop();
                    prompt.pop();
                    prompt.push_str(": ");
                    prompt
                };
                *ctx.learn_instance.lock().unwrap() = Some(LearnInstance {
                    set: set_name,
                    curr_word: initial.0,
                    curr_val: initial.1,
                    success: 0,
                    failed: 0,
                });
                ctx.cmd_line.push_screen(Window::new(CLIBuilder::new().command(
                    CommandBuilder::new("$end", CmdLearnEnd)
                ).fallback(Box::new(InputFallback)).prompt(prompt)));
                return Ok(());
            }
        }
        return Err(anyhow::Error::from(SetDoesNotExistsError(set_name)));
    }
}

struct CmdLearnEnd;

impl CommandImpl for CmdLearnEnd {
    type CTX = Arc<TrainingCtx>;

    fn execute(&self, ctx: &Arc<TrainingCtx>, _input: &[&str]) -> anyhow::Result<()> {
        ctx.cmd_line.pop_screen();
        let instance = ctx.learn_instance.lock().unwrap();
        let stats = instance.as_ref().unwrap();
        ctx.cmd_line.println(&format!("You learned {}/{} vocabs successfully ({:.2}%)", stats.success, stats.success + stats.failed, (stats.success as f64 / (stats.success as f64 + stats.failed as f64))));
        Ok(())
    }
}

struct InputFallback;

impl FallbackHandler for InputFallback {
    fn handle(&self, input: String, window: &Window, ctx: &Arc<TrainingCtx>) -> anyhow::Result<bool> {
        let mut learn_instance = ctx.learn_instance.lock().unwrap();
        if let Some(learn_instance) = learn_instance.as_mut() {
            let mut set = ctx.sets.get_mut(&learn_instance.set).unwrap();
            {
                let mut config = set.config.as_mut().unwrap();
                let mut meta = set.meta.lock().unwrap();
                meta.tries += 1;
                let tries = meta.tries;
                let mut entry = meta.entries.get_mut(&learn_instance.curr_word.0).unwrap();
                entry.tries += 1;
                entry.last_presented = tries;
                if learn_instance.curr_val.matches(input.trim()) {
                    if learn_instance.curr_val.has_multiple_solutions() {
                        window.println(&format!("\"{}\" is correct, among others {}", input, learn_instance.curr_val).green().to_string());
                    } else {
                        window.println(&format!("\"{}\" is correct", input).green().to_string());
                    }
                    learn_instance.success += 1;
                    entry.successes += 1;
                    drop(entry);
                    meta.successes += 1;
                } else {
                    window.println(&format!("\"{}\" is wrong, correct answers are {}", input, learn_instance.curr_val).red().to_string());
                    learn_instance.failed += 1;
                }
            }
            set.save_meta();
            let word = set.pick_word();
            let prompt = {
                let mut prompt = String::new();
                for key in word.0.1.iter() {
                    prompt.push_str(key);
                    prompt.push_str(", ");
                }
                prompt.pop();
                prompt.pop();
                prompt.push_str(": ");
                prompt
            };
            window.set_prompt(prompt);
            learn_instance.curr_word = word.0;
            learn_instance.curr_val = word.1;
        }
        Ok(true)
    }
}
