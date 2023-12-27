#![feature(string_remove_matches)]

use std::{io::Write, fs, sync::{Arc, Mutex}, collections::HashMap, ops::Deref, error::Error, fmt::Display, path::PathBuf};

use cli_core::CommandImpl;
use cmd_line::{Window, CmdLineInterface, FallbackHandler};
use colored::{Colorize, ColoredString};
use config::{LearnSetConfig, Set, WordValue};
use utils::{input, count_occourances};

use crate::{cmd_line::{CLIBuilder, PrintFallback}, config::{LearnSetMeta, Questioning, Amount, Direction}, cli_core::{CommandBuilder, CommandParamTy, CmdParamStrConstraints, UsageBuilder, CommandParam}};

mod utils;
mod config;
mod cmd_line;
mod cli_core;

fn dir() -> PathBuf {
    // dirs::executable_dir().unwrap().join("./VocabTrainer")
    PathBuf::from("./VocabTrainer")
}

fn main() -> anyhow::Result<()> {
    let dir = dir();
    println!("dir: {:?}", dir);
    std::fs::create_dir_all(&dir)?;
    println!("got dir!");
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
            if extension.eq_ignore_ascii_case("txt") {
                // check if there is no cfg for this set
                if !config.exists() {
                    sets.push(Set {
                        name: file_name.to_string(),
                        kind: config::SetKind::Unconfigured,
                        config: None,
                    });
                    continue;
                }
                let cfg: LearnSetConfig = serde_json::from_slice(&fs::read(config)?)?;
                let mut content = fs::read_to_string(dir.join(format!("{}.txt", &file_name)))?;
                content.remove_matches('\r');
                let entries = content.split('\n').collect::<Vec<&str>>();
                match cfg.mode {
                    config::QuestioningMode::Order => {
                        if count_occourances(&content, cfg.kv_seperator) == entries.len() {
                            let mut err = false;
                            let set = Set {
                                name: file_name.to_string(),
                                kind: config::SetKind::Order(entries.iter().map(|s| {
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
                            }).collect::<Vec<_>>()),
                                config: Some(cfg.clone()),
                            };
                            if !err {
                                sets.push(set);
                                continue;
                            }
                        }
                        sets.push(Set {
                            name: file_name.to_string(),
                            kind: config::SetKind::Order(entries.iter().enumerate().map(|(i, s)| (i, s.trim().to_string())).collect::<Vec<_>>()),
                            config: Some(cfg),
                        });
                    },
                    _ => {
                        let mut err = false;
                        let pairs = entries.iter().enumerate().map(|(i, s)| {
                            if let Some((left, right)) = s.split_once(cfg.kv_seperator) {
                                let left = left.trim();
                                let right = right.trim();
                                (left.split(',').map(|s| s.trim().to_owned()).collect::<Vec<_>>(), right.split(',').map(|s| s.trim().to_owned()).collect::<Vec<_>>())
                            } else {
                                if !cfg.ignore_errors {
                                    err = true;
                                    println!("Set {file_name}: missing `{}` in line {i}", cfg.kv_seperator);
                                }
                                (vec![], vec![])
                            }
                        }).filter(|e| !e.0.is_empty() && !e.1.is_empty()).collect::<Vec<_>>();
                        if !err {
                            sets.push(Set {
                                name: file_name.to_string(),
                                kind: config::SetKind::KV(pairs),
                                config: Some(cfg),
                            });
                        }
                    }
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
        ).fallback(Box::new(PrintFallback("Please use `help` in order to learn which commands are available".red()))))),
        sets: {
            let mut map = HashMap::new();
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
    Ok(())
}

pub struct TrainingCtx {
    cmd_line: CmdLineInterface,
    sets: HashMap<String, Set>,
    learn_instance: Mutex<Option<LearnInstance>>,
}

struct LearnInstance {
    set: String,
    curr_word: Vec<String>,
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
            ctx.cmd_line.println(&format!("Found {}: {:?}", set.1.name, set.1.kind)).unwrap();
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
        if !dir.join(format!("{}.txt", &set_name)).exists() {
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
            meta: LearnSetMeta {},
            kv_seperator: ':',
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
                    for key in initial.0.iter() {
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
        todo!()
        // return Err(());
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
            if learn_instance.curr_word.contains(&input.to_lowercase()) {
                window.println(&format!("{} is correct, among others {}", input, learn_instance.curr_val).green());
                learn_instance.success += 1;
            } else {
                window.println(&format!("{} is wrong, the correct answer is {}", input, learn_instance.curr_val).red());
                learn_instance.failed += 1;
            }
            let word = ctx.sets.get(&learn_instance.set).unwrap().pick_word();
            let prompt = {
                let mut prompt = String::new();
                for key in word.0.iter() {
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
