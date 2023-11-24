use std::env::args_os;
use std::ffi::OsString;
use masterpg::compose;
use string_io_and_mock::FileTextHandler;

fn main() {
    let sources = &args_os().collect::<Vec<OsString>>()[1..];

    if sources.is_empty() {
        show_usage();
        return;
    }

    for source_path in sources {
        println!("Processing source path : {:?}", source_path);

        let mut file_handler = FileTextHandler::new();
        let result = compose(source_path, &mut file_handler);

        match result {
            Ok(_) => println!("{:?} processed succesfully.", source_path),
            Err(err) => println!("Error while processing {:?} : {}", source_path, err),
        }
    }
}

fn show_usage() {
    let usage_text = "
Usage: masterpg [SOURCE_FILE] ...

For each SOURCE_FILE, composes an output file as specified by
the <+output .../> and other tags found in that source file.

SOURCE_FILE is the relative or absolute path to a file
that holds masterpg tags, may or may not refer to
other source files and will result in the composition
of an output file by masterpg.
".trim();

    println!("{}", usage_text);
}
