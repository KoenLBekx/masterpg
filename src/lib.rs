use std::ffi::OsStr;
use nom::{
    branch::alt,
    bytes::complete::{is_a, is_not, tag, take},
    character::complete::char,
    combinator::{map, opt, peek, recognize},
    Err,
    error::{Error, ErrorKind},
    IResult,
    multi::{many0, many1, separated_list0},
    Parser,
    sequence::{delimited, pair, preceded, terminated}
};
use string_io_and_mock::TextIOHandler;

enum Operand {
    PlaceHolder(String),
    Value(f64),
}

struct Calculation {
    operator: String,
    operand1: Operand,
    operand2: Operand,
}

enum MasterPageTag {
    Comment,
    Output(String),
    Master(String),
    Actual(Vec<PageContent>),
    Calc(Calculation),
    PlaceHolder(String),
}

enum PageContent {
    MPTag(MasterPageTag),
    Other(String),
}

fn whitespace(input: &str)-> IResult<&str, &str> {
    is_a(" \t\n\r")(input)
}

fn master_page_tag_opening_content(input: &str, self_contained:bool) -> IResult<&str, Vec<String>> {
    let closing_mark = match self_contained {
        true => "/>",
        false => ">",
    };

    delimited(
        terminated(tag("<+"), opt(whitespace)),
        map(
            pair(
                terminated(
                    is_not(" \t\n\r/><"),
                    is_a(" \t\n\r")
                ),
                separated_list0(
                    is_a(" \t\n\r"),
                    map(
                        many0(
                            alt((
                                is_not(" \t\n\r/><"),
                                terminated(tag("/"), peek(is_not(">")))
                            ))
                        ),
                        |list: Vec<&str>|
                        {
                            let mut result_str = String::new();

                            for elem in list {
                                result_str.push_str(elem);
                            }

                            result_str
                        }
                    )
                ),
            ),
            |(first, list):(&str, Vec<String>)|
            {
                let mut result_vec = vec![];
                result_vec.push(first.to_string());

                for elem in list {
                    if elem.len() > 0 {
                        result_vec.push(elem);
                    }
                }

                result_vec
            }
        ),
        preceded(opt(whitespace), tag(closing_mark))
    )(input)
}

fn master_page_closing_tag(input: &str) -> IResult<&str, String> {
    map(
        delimited(
            terminated(tag("</+"), opt(whitespace)),
            is_not(" \t\n\r/><"),
            preceded(opt(whitespace), tag(">"))
        ),
        |found| String::from(found)
    )(input)
}

/*
fn master_page_opening_closing_tag(input: &str) -> IResult<&str, (Vec<String>,  String)> {
    pair(
    )(input)
}
*/

pub fn compose<Tioh>(file_path:&OsStr, io_handler:Tioh) -> Result<(), Box<dyn std::error::Error>>
where Tioh: TextIOHandler {
    let all_texts:String = io_handler.read_text(file_path)?;
    
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn master_page_tag_self_contained_empty() {
        assert!(master_page_tag_opening_content("<+ />", true).is_err());
    }

    #[test]
    fn master_page_tag_opening_empty() {
        assert!(master_page_tag_opening_content("<+>", true).is_err());
    }

    #[test]
    fn master_page_tag_self_contained_single_content() {
        assert_eq!(
            Ok(("", vec!["master".to_string()])),
            master_page_tag_opening_content("<+master />", true)
        );
    }

    #[test]
    fn master_page_tag_self_contained_double_content() {
        assert_eq!(
            Ok(("", vec!["master".to_string(), "general.mpm".to_string()])),
            master_page_tag_opening_content("<+master    general.mpm/>", true)
        );
    }

    #[test]
    fn master_page_tag_opening_double_content() {
        assert_eq!(
            Ok(("", vec!["actual".to_string(), "header".to_string()])),
            master_page_tag_opening_content("<+actual header>", false)
        );
    }

    #[test]
    fn master_page_tag_self_contained_line_break() {
        assert_eq!(
            Ok(("", vec!["master".to_string(), "general.mpm".to_string()])),
            master_page_tag_opening_content("<+master\ngeneral.mpm/>", true)
        );
    }

    #[test]
    fn master_page_tag_self_contained_surrounding_spaces() {
        assert_eq!(
            Ok(("", vec!["master".to_string(), "general.mpm".to_string()])),
            master_page_tag_opening_content("<+ master general.mpm  />", true)
        );
    }

    #[test]
    fn master_page_tag_self_contained_text_after() {
        assert_eq!(
            Ok(("and some more", vec!["master".to_string(), "general.mpm".to_string()])),
            master_page_tag_opening_content("<+master general.mpm/>and some more", true)
        );
    }

    #[test]
    fn master_page_tag_self_contained_single_slash() {
        assert_eq!(
            Ok(("", vec!["master".to_string(), "/".to_string(), "general.mpm".to_string()])),
            master_page_tag_opening_content("<+master / general.mpm/>", true)
        );
    }

    #[test]
    fn master_page_tag_self_contained_no_slash_in_first_word() {
        /*
        // The test succeeds also this way, but we're not interested in the specifics of the error.
        // Furthermore, we don't want to rewrite this test if the nom crate changes its error
        // generation.
        assert_eq!(
            Err(nom::Err::Error(nom::error::Error { input: "/ter general.mpm/>", code: ErrorKind::IsA })),
            master_page_tag_opening_content("<+mas/ter general.mpm/>", true)
        );
        */

        assert!(master_page_tag_opening_content("<+mas/ter general.mpm/>", true).is_err());
    }

    #[test]
    fn master_page_tag_self_contained_double_slash() {
        assert_eq!(
            Ok(("", vec!["master".to_string(), "//".to_string(), "general.mpm".to_string()])),
            master_page_tag_opening_content("<+master // general.mpm/>", true)
        );
    }

    #[test]
    fn master_page_tag_self_contained_slash_in_word() {
        assert_eq!(
            Ok(("", vec!["master".to_string(), "a/a//azz".to_string(), "general.mpm".to_string()])),
            master_page_tag_opening_content("<+master a/a//azz general.mpm/>", true)
        );
    }

    #[test]
    fn master_page_tag_self_contained_non_greedy() {
        assert_eq!(
            Ok((" general.mpm/>", vec!["master".to_string()])),
            master_page_tag_opening_content("<+master /> general.mpm/>", true)
        );
    }

    #[test]
    fn master_page_closing_tag_no_spaces() {
        assert_eq!(
            Ok(("", "actual".to_string())),
            master_page_closing_tag("</+actual>")
        );
    }

    #[test]
    fn master_page_closing_tag_spaces() {
        assert_eq!(
            Ok(("", "actual".to_string())),
            master_page_closing_tag("</+ \t actual >")
        );
    }

    #[test]
    fn master_page_closing_tag_text_after() {
        assert_eq!(
            Ok(("Hello everybody, ...", "actual".to_string())),
            master_page_closing_tag("</+ \t actual >Hello everybody, ...")
        );
    }

    #[test]
    fn master_page_closing_tag_non_greedy() {
        assert_eq!(
            Ok(("ity>", "actual".to_string())),
            master_page_closing_tag("</+actual>ity>")
        );
    }

    #[test]
    fn master_page_closing_tag_slash() {
        /*
        assert_eq!(
            Err(nom::Err::Error(nom::error::Error { input: "/tual >", code: ErrorKind::Tag })),
            master_page_closing_tag("</+ \t ac/tual >")
        );
        */

        assert!(master_page_closing_tag("</+ \t ac/tual >").is_err());
    }

    mod enveloppe_same_name {
        use std::cell::RefCell;
        use std::rc::Rc;
        use nom::{
            branch::alt,
            bytes::complete::{is_not, tag, take},
            character::complete::alphanumeric1,
            combinator::{map, verify},
            error::Error,
            IResult,
            multi::many0,
            sequence::{preceded, terminated, tuple},
        };

        macro_rules! make_open_mark {
            ($name_stack:expr, $is_first:expr) => {
                verify(
                    preceded(tag::<&str, &str, Error<&str>>("<"), take(1u8)),
                    |name: &str| {
                        match name {
                            "<" | ">" => false,
                            nm =>  {
                                if $is_first && ($name_stack.borrow().len() > 0) {
                                    return false;
                                }

                                if (!$is_first) && ($name_stack.borrow().len() < 1) {
                                    return false;
                                }

                                $name_stack.borrow_mut().push(name.to_string());

                                true
                            }
                        }
                    }
                )
            }
        }

        macro_rules! make_close_mark {
            ($name_stack:expr, $is_last:expr) => {
                verify(
                    preceded(tag::<&str, &str, Error<&str>>(">"), take(1u8)),
                    |name: &str|
                    {
                        match name {
                            "<" | ">" => false,
                            nm =>  {
                                let mut stack_mut = $name_stack.borrow_mut();

                                // println!("Before verify on close : name={}, name_stack={:?}", nm, stack_mut);

                                if $is_last && (stack_mut.len() != 1) {
                                    return false;
                                }

                                if (!$is_last) && (stack_mut.len() < 2) {
                                    return false;
                                }

                                match stack_mut.last() {
                                    None => false,
                                    Some(ref last_name) => {
                                        if nm == last_name.as_str() {
                                            stack_mut.pop();
                                            true
                                        } else {
                                            false
                                        }
                                    }
                                }
                            }
                        }
                    }
                )
            }
        }

        #[test]
        fn nom_open_close_same_name() {
            /* Given opening and closing marks of the form
             * <alpha and >alpha,
             * checks if
             * <a___>a
             * is recognized as one entity, but not
             * <a___>b
             * while
             * <a___<b___>b___>a
             * is recognized as such again.
             */

            let name_stack: RefCell<Vec<String>> = RefCell::new(Vec::new());
            
            let mut open_mark_first = make_open_mark!(name_stack, true);
            let mut open_mark_nested = make_open_mark!(name_stack, false);

            assert_eq!(Ok(("12", "a")), open_mark_first("<a12"));

            let mut close_mark_last = make_close_mark!(name_stack, true);
            let mut close_mark_nested = make_close_mark!(name_stack, false);

            assert_eq!(Ok(("zz", "a")), close_mark_last(">azz"));

            let mut enveloppe = map(
                tuple((
                    open_mark_first,
                    many0(alt((
                        is_not("<>"),
                        open_mark_nested,
                        close_mark_nested,
                    ))),
                    close_mark_last
                )),
                |tp|
                {
                    let mut result = Vec::<&str>::new();
                    result.push(tp.0);
                    result.extend(tp.1.iter());
                    result.push(tp.2);

                    result
                }
            );

            name_stack.borrow_mut().clear();
            assert_eq!(
                Ok(("", vec!["x", "_-_-_", "x"])),
                enveloppe("<x_-_-_>x")
            );

            name_stack.borrow_mut().clear();
            assert_eq!(
                Ok(("", vec!["x", "_", "x", "_", "x", "x"])),
                enveloppe("<x_<x_>x>x")
            );

            name_stack.borrow_mut().clear();
            assert_eq!(
                Ok(("", vec!["x", "x"])),
                enveloppe("<x>x")
            );

            name_stack.borrow_mut().clear();
            assert_eq!(
                Ok(("", vec!["x", "_", "y", "-", "y", "_", "x"])),
                enveloppe("<x_<y->y_>x")
            );

            name_stack.borrow_mut().clear();
            assert_eq!(
                Ok(("---<a--->a", vec!["x", "_", "y", "-", "y", "_", "x"])),
                enveloppe("<x_<y->y_>x---<a--->a")
            );

            name_stack.borrow_mut().clear();
            assert!(enveloppe("___<").is_err());

            name_stack.borrow_mut().clear();
            assert!(enveloppe("<<___><").is_err());

            name_stack.borrow_mut().clear();
            assert!(enveloppe("<x___").is_err());

            name_stack.borrow_mut().clear();
            assert!(enveloppe(">x").is_err());

            name_stack.borrow_mut().clear();
            assert!(enveloppe("<x___>y").is_err());

            name_stack.borrow_mut().clear();
            assert!(enveloppe("<x__<x__>x").is_err());

            name_stack.borrow_mut().clear();
            assert!(enveloppe("<x__<z__>x").is_err());

            name_stack.borrow_mut().clear();
            assert!(enveloppe("<x__>z__>x").is_err());

            name_stack.borrow_mut().clear();
            assert!(enveloppe("<x__<z__>x__>z").is_err());
        }
    }
}
