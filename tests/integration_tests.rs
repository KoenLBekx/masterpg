use masterpg::compose;
use std::ffi::OsString;
use string_io_and_mock::{TextIOHandler, FileTextHandler};
use serial_test::file_serial;

mod utils;

#[test]
#[file_serial]
fn compose_with_master() {
    let playground_name = utils::ensure_playground(true);
    let mut input_file_handler = FileTextHandler::new();

    let consts_source = "
<+actual siteName>Test Pages</+actual>
<+actual author>Lenny Baxter</+actual>
".to_string();

    let mut consts_name = playground_name.clone();
    consts_name.push(&OsString::from("/consts.mpm"));
    input_file_handler.write_text(&consts_name, consts_source).unwrap();

    let general_source = "
<+master tests/playground/consts.mpm/>
<!doctype html>
<html>
<head>
<title><+placeholder pageTitle/></title>
</head>
<body>
<h1><+placeholder pageTitle/></h1>
<i>by <+placeholder author/></i>
<div id=\"main\"><+placeholder pageContent/></div>
</body>
</html>
".to_string();

    let mut general_name = playground_name.clone();
    general_name.push(&OsString::from("/general.mpx"));
    input_file_handler.write_text(&general_name, general_source).unwrap();

let page_source = "
<+master tests/playground/general.mpx/>
<+output tests/playground/index.htm/>
<+actual pageContent>
<p>Welcome to my <+placeholder siteName/> site !</p>
<p>I'm <+placeholder author/> and I'm just testing the masterpg module.</p>
<p>Bye for now !</p>
</+actual>
<+actual pageTitle>Welcome</+actual>
".to_string();

    let mut testpg_name = playground_name.clone();
    testpg_name.push(&OsString::from("/testpg.mpc"));
    input_file_handler.write_text(&testpg_name, page_source).unwrap();

    let mut process_file_handler = FileTextHandler::new();
    let result = compose(&testpg_name, &mut process_file_handler);

    // Debug
    // println!("{}", result.unwrap_err());

    assert!(result.is_ok());

    let result_file_handler = FileTextHandler::new();
    let mut result_name = playground_name.clone();
    result_name.push(&OsString::from("/index.htm"));

    let result_text = result_file_handler.read_text(&result_name).unwrap();

    // Debug
    /*
    println!("
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Composition result :
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{}",
        result_text);
    */

    let expected = "
<!doctype html>
<html>
<head>
<title>Welcome</title>
</head>
<body>
<h1>Welcome</h1>
<i>by Lenny Baxter</i>
<div id=\"main\">
<p>Welcome to my Test Pages site !</p>
<p>I'm Lenny Baxter and I'm just testing the masterpg module.</p>
<p>Bye for now !</p>
</div>
</body>
</html>
".trim();

    assert_eq!(expected.to_string(), result_text);
}

