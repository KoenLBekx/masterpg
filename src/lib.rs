use std::ffi::OsStr;
use nom::{
    branch::alt,
    bytes::complete::{is_a, is_not, tag},
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
    Actual(String),
    Calc(Calculation),
    PlaceHolder(String),
}

enum PageContent {
    Tag(MasterPageTag),
    Other(String),
}

fn whitespace(input: &str)-> IResult<&str, &str> {
    is_a(" \t\n\r")(input)
}

fn master_page_tag_no_children_content(input: &str) -> IResult<&str, Vec<String>> {
    delimited(
        terminated(tag("<+"), opt(whitespace)),
        map(
            pair(
                terminated(
                    is_not(" \t\n\r/>"),
                    is_a(" \t\n\r")
                ),
                separated_list0(
                    is_a(" \t\n\r"),
                    map(
                        many0(
                            alt((
                                is_not(" \t\n\r/>"),
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
        preceded(opt(whitespace), tag("/>"))
    )(input)
}

fn master_page_closing_tag(input: &str) -> IResult<&str, String> {
    map(
        delimited(
            terminated(tag("</+"), opt(whitespace)),
            is_not(" \t\n\r/>"),
            preceded(opt(whitespace), tag(">"))
        ),
        |found| String::from(found)
    )(input)
}

pub fn compose<Tioh>(file_path:&OsStr, io_handler:Tioh) -> Result<(), Box<dyn std::error::Error>>
where Tioh: TextIOHandler {
    let all_texts:String = io_handler.read_text(file_path)?;
    
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn master_page_tag_no_children_single_content() {
        assert_eq!(
            Ok(("", vec!["master".to_string()])),
            master_page_tag_no_children_content("<+master />")
        );
    }

    #[test]
    fn master_page_tag_no_children_double_content() {
        assert_eq!(
            Ok(("", vec!["master".to_string(), "general.mpm".to_string()])),
            master_page_tag_no_children_content("<+master    general.mpm/>")
        );
    }

    #[test]
    fn master_page_tag_no_children_line_break() {
        assert_eq!(
            Ok(("", vec!["master".to_string(), "general.mpm".to_string()])),
            master_page_tag_no_children_content("<+master\ngeneral.mpm/>")
        );
    }

    #[test]
    fn master_page_tag_no_children_surrounding_spaces() {
        assert_eq!(
            Ok(("", vec!["master".to_string(), "general.mpm".to_string()])),
            master_page_tag_no_children_content("<+ master general.mpm  />")
        );
    }

    #[test]
    fn master_page_tag_no_children_text_after() {
        assert_eq!(
            Ok(("and some more", vec!["master".to_string(), "general.mpm".to_string()])),
            master_page_tag_no_children_content("<+master general.mpm/>and some more")
        );
    }

    #[test]
    fn master_page_tag_no_children_single_slash() {
        assert_eq!(
            Ok(("", vec!["master".to_string(), "/".to_string(), "general.mpm".to_string()])),
            master_page_tag_no_children_content("<+master / general.mpm/>")
        );
    }

    #[test]
    fn master_page_tag_no_children_no_slash_in_first_word() {
        /*
        // The test succeeds also this way, but we're not interested in the specifics of the error.
        // Furthermore, we don't want to rewrite this test if the nom crate changes its error
        // generation.
        assert_eq!(
            Err(nom::Err::Error(nom::error::Error { input: "/ter general.mpm/>", code: ErrorKind::IsA })),
            master_page_tag_no_children_content("<+mas/ter general.mpm/>")
        );
        */

        assert!(master_page_tag_no_children_content("<+mas/ter general.mpm/>").is_err());
    }

    #[test]
    fn master_page_tag_no_children_double_slash() {
        assert_eq!(
            Ok(("", vec!["master".to_string(), "//".to_string(), "general.mpm".to_string()])),
            master_page_tag_no_children_content("<+master // general.mpm/>")
        );
    }

    #[test]
    fn master_page_tag_no_children_slash_in_word() {
        assert_eq!(
            Ok(("", vec!["master".to_string(), "a/a//azz".to_string(), "general.mpm".to_string()])),
            master_page_tag_no_children_content("<+master a/a//azz general.mpm/>")
        );
    }

    #[test]
    fn master_page_tag_no_children_non_greedy() {
        assert_eq!(
            Ok((" general.mpm/>", vec!["master".to_string()])),
            master_page_tag_no_children_content("<+master /> general.mpm/>")
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
}
