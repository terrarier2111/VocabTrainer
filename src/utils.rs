use std::hash::{DefaultHasher, Hasher};

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
pub fn calculate_hash<T: std::hash::Hash>(t: &T) -> u64 {
    let mut s = DefaultHasher::new();
    t.hash(&mut s);
    s.finish()
}
