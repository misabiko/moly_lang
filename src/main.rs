use std::path::PathBuf;
use clap::{Parser, Subcommand};
use moly::repl;

fn main() {
	let cli = Cli::parse();

	match &cli.command {
		None | Some(Commands::Repl) => repl::start(),
		Some(Commands::Run { input }) => println!("TODO: Run `{:?}`", input),
		Some(Commands::Exec { input }) => moly::run_string(input),
	}

}

#[derive(Parser)]
#[clap(author, about, long_about = None)]
struct Cli {
	#[clap(subcommand)]
	command: Option<Commands>,
}

#[derive(Subcommand)]
enum Commands {
	Repl,
	/// Run the given file
	Run {
		//TODO Make optional
		#[clap(value_parser)]
		input: PathBuf,
	},
	/// Parse and execute a string of code directly
	Exec {
		#[clap(value_parser)]
		input: String,
	},
}