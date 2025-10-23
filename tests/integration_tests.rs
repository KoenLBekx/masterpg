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

#[test]
#[file_serial]
fn compose_svg_in_html() {
    let playground_name = utils::ensure_playground(true);
    let mut input_file_handler = FileTextHandler::new();

    let routines_source = "
        <+laconic routineDeclarations {
            R(
                #toSvgDattribute

                [c Read coordinates from stack]
                $#nrItems k,
                $#baseName #item

                F
                    1
                    v#nrItems
                    1
                    #ix
                    $+,v#baseName v#ix k

                $#minx 10_000_000
                $#maxx ~v#minx
                $#miny v#minx
                $#maxy v#maxx

                [c Get min and max value of odd (x) coordinates]
                F(
                    1
                    v#nrItems
                    2
                    #ix
                    $#coord v+,v#baseName v#ix
                    m :#minx v#coord
                    M :#maxx v#coord
                )

                [c Get min and max value of even (y) coordinates]
                F(
                    0
                    v#nrItems
                    2
                    #ix
                    $#coord v+,v#baseName v#ix
                    m :#miny v#coord
                    M :#maxy v#coord
                )

                [c Compose d attribute value]
                $#d #

                F
                    1
                    v#nrItems
                    1
                    #ix
                    ;(
                        +
                            :#d
                            ?
                                =v#ix 1
                                [sM ]
                                ?
                                    =1 %v#ix 2
                                    [sL ]
                                    #
                            
                        +(
                            :#d
                            v+,
                                v#baseName
                                v#ix
                            [s ]
                        )
                    )

                v#d
            )
        }/>
    ".to_string();

    let mut routines_src_name = playground_name.clone();
    routines_src_name.push(&OsString::from("/routines_src.mpm"));
    input_file_handler.write_text(&routines_src_name, routines_source).unwrap();

    let page_source = r#"
        <+master tests/playground/routines_src.mpm/>
        <+output tests/playground/testWithSvg.htm/>
        <!doctype html>
        <html>
        <head>
            <title>Test SVG</title>
            <style type="text/css">
                html,
                body,
                svg {
                    height: 100%;
                    vertical-align: top;
                }
                svg {
                    display: inline-block;
                }
            </style>
        </head>
        <body>
        <h1>Some graph</h1>
            <svg viewBox="0 0 100 100" xmlns="http://www.w3.org/2000/svg">
                <path
                    d="
                        <+laconic - {
                            F
                                10
                                0
                                1
                                #ix
                                ;
                                    K ^ v#ix 1.5
                                    K v#ix

                            X#toSvgDattribute
                        }/>
                    "
                    fill="red" stroke="blue" stroke-width="3"
                />
            </svg>
        </body>
        </html>
    "#.to_string();

    let mut page_src_name = playground_name.clone();
    page_src_name.push(&OsString::from("/page_src.mpc"));
    input_file_handler.write_text(&page_src_name, page_source).unwrap();

    let mut process_file_handler = FileTextHandler::new();
    let result = compose(&page_src_name, &mut process_file_handler);

    // Debug
    // println!("{}", result.clone().unwrap_err());

    assert!(result.is_ok());

    let result_file_handler = FileTextHandler::new();
    let mut result_name = playground_name.clone();
    result_name.push(&OsString::from("/testWithSvg.htm"));

    let result_text = result_file_handler.read_text(&result_name).unwrap();

    // Debug
    println!("
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Composition result :
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{}",
        result_text);
}

#[test]
#[file_serial]
fn laconic_errors() {
    let playground_name = utils::ensure_playground(true);
    let mut input_file_handler = FileTextHandler::new();

    let page_source = r#"
        <+output tests/playground/composed.txt/>
        <+laconic - {/ 588 0}/>
    "#.to_string();

    let mut page_src_name = playground_name.clone();
    page_src_name.push(&OsString::from("/page_src.mpc"));
    input_file_handler.write_text(&page_src_name, page_source).unwrap();

    let mut process_file_handler = FileTextHandler::new();
    let result = compose(&page_src_name, &mut process_file_handler);

    // Debug
    // println!("{}", result.clone().unwrap_err());

    assert!(result.is_ok());
    let errors_found = result.unwrap();
    assert_eq!(1_usize, errors_found.len());
    assert!(errors_found[0].contains("DivideByZero"));

    let result_file_handler = FileTextHandler::new();
    let mut result_name = playground_name.clone();
    result_name.push(&OsString::from("/composed.txt"));

    let result_text = result_file_handler.read_text(&result_name).unwrap();

    // Debug
    println!("
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Composition result :
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{}",
        result_text);
}

