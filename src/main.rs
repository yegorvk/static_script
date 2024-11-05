use anyhow::Result;
use static_script::runner::run_program;
use std::io;
use std::io::Write;

fn main() -> Result<()> {
    #[cfg(feature = "runtime")]
    {
        run_interactive()
    }

    #[cfg(not(feature = "runtime"))]
    {
        eprintln!("Feature `runtime` is not enabled for this build.");
    }
}

#[cfg(feature = "runtime")]
fn run_interactive() -> Result<()> {
    let mut expr = String::new();

    loop {
        print!(">> ");
        io::stdout().flush()?;

        expr.clear();
        io::stdin().read_line(&mut expr)?;

        match run_program(&expr, &[], &[]) {
            Ok(val) => {
                println!("Result: {val}.");
            }
            Err(e) => {
                println!("{e}");
            }
        }
    }
}
