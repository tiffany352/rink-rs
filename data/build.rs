use std::{env, path::PathBuf};

fn main() -> eyre::Result<()> {
    color_eyre::install()?;

    let mut path = PathBuf::from(env::var("OUT_DIR")?);
    path.push("rink-data.bincode");

    let contents = rink_data_compiler::compile()?;
    std::fs::write(path, contents)?;

    Ok(())
}
