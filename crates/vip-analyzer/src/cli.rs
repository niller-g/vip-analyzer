//! Various batch processing tasks, intended primarily for debugging.

pub mod flags;

use std::io::Read;

#[expect(unused)]
fn read_stdin() -> anyhow::Result<String> {
    let mut buff = String::new();
    std::io::stdin().read_to_string(&mut buff)?;
    Ok(buff)
}
