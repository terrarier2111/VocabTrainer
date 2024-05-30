use std::{
    fmt::Display,
    hash::{DefaultHasher, Hasher},
    io::{Error, ErrorKind, Write},
};

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

#[inline]
pub fn four_to_pow(pow: usize) -> usize {
    if pow >= (usize::BITS as usize / 2) {
        return 0;
    }
    1 << (2 * pow)
}

#[inline]
pub fn largest_pow_of_four(val: usize) -> usize {
    let highest_set_bit = usize::BITS as usize - val.max(1).leading_zeros() as usize;
    highest_set_bit.div_ceil(2)
}

#[inline]
pub fn calculate_hash<T: std::hash::Hash>(t: &T) -> u64 {
    let mut s = DefaultHasher::new();
    t.hash(&mut s);
    s.finish()
}
