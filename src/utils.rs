use std::{io::{Error, ErrorKind, Write}, fmt::Display};


pub fn input<F: Display>(text: F) -> anyhow::Result<String> {
    print!("{}", text);
    std::io::stdout().flush()?; // because print! doesn't flush
    let mut input = String::new();
    if std::io::stdin().read_line(&mut input)? == 0 {
        return anyhow::Result::Err(anyhow::Error::from(Error::new(
            ErrorKind::UnexpectedEof,
            "EOF while reading a line",
        )));
    }
    if input.ends_with('\n') {
        input.pop();
        if input.ends_with('\r') {
            input.pop();
        }
    }
    Ok(input)
}

pub fn count_occourances(src: &str, chr: char) -> usize {
    let mut occs = 0;
    for c in src.chars() {
        if c == chr {
            occs += 1;
        }
    }
    occs
}