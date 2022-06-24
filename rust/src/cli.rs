use clap::{Arg, Command};

pub fn get_args() -> Option<String> {
    let matches = Command::new("Lox")
        .author("@seaerchin")
        .version("0.1")
        .arg(
            Arg::new("in_file")
                .help("the optional entrypoint of the Lox app")
                .required(false),
        )
        .get_matches();

    matches
        .get_one("in_file")
        .map(|file_name: &String| file_name.to_owned())
}
