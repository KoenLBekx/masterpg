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

        use std::cell::{Ref, RefCell};
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

        const OPEN_MARK: &str = "<";
        const CLOSE_MARK: &str = ">";
        const NAME_LENGTH: u8 = 1;

        #[derive(Debug)]
        #[derive(PartialEq)]
        enum Fragment<'a> {
            OpenMark(&'a str),
            CloseMark,
            Other(&'a str),
        }

        use Fragment::{OpenMark, CloseMark, Other};

        macro_rules! make_open_mark {
            ($name_stack:expr, $is_first:expr) => {
                map(
                    verify(
                        preceded(tag::<&str, &str, Error<&str>>(OPEN_MARK), take(NAME_LENGTH)),
                        |name: &str| {
                            match name {
                                OPEN_MARK | CLOSE_MARK => false,
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
                    ),
                    |name| OpenMark(name)
                )
            }
        }

        macro_rules! make_close_mark {
            ($name_stack:expr, $is_last:expr) => {
                map(
                    verify(
                        preceded(tag::<&str, &str, Error<&str>>(CLOSE_MARK), take(NAME_LENGTH)),
                        |name: &str|
                        {
                            match name {
                                OPEN_MARK | CLOSE_MARK => false,
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
                    ),
                    |_| CloseMark
                )
            }
        }

        enum NestedContent {
            Enveloppe(Rc<RefCell<Enveloppe>>),
            Other(String),
        }
        impl std::fmt::Display for NestedContent {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                let result: String = match self {
                    // NestedContent::Enveloppe(ref env_cell) => EnveloppeCellWrapper{value: Rc::clone(&env_cell)}.to_string(),
                    NestedContent::Enveloppe(ref env_cell) => env_cell.displayable().to_string(),
                    NestedContent::Other(ref theString) => theString.clone(),
                };

                write!(f, "{}", result)
            }
        }

        struct Enveloppe {
            name: String,
            children: Vec<NestedContent>,
            is_open: bool,
        }

        trait EnveloppeCellFunctions {
            fn create(name: String) -> Self;
            fn close(&self);
            fn add_fragment(&self, fragment: Fragment) -> Result<(), String>;
            fn displayable(&self) -> EnveloppeCellWrapper;
        }

        // Cfr. The Rust Book, Chapter 19.2, "Using the Newtype Pattern to Implement External Traits on External Types"
        struct EnveloppeCellWrapper {
            value: Rc<RefCell<Enveloppe>>
        }
        impl std::ops::Deref for EnveloppeCellWrapper {
            type Target = Rc<RefCell<Enveloppe>>;

            fn deref(&self) -> &Self::Target {
                &self.value
            }
        }
        impl std::fmt::Display for EnveloppeCellWrapper {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                let enveloppe = self.value.borrow();
                let mut result = format!(
                    "{}{}{}{}{}",
                    OPEN_MARK,
                    enveloppe.name.clone(),
                    enveloppe.children.iter().fold(String::new(), |acc, nested| {
                        format!("{}{}", acc, nested.to_string())
                    }),
                    CLOSE_MARK,
                    enveloppe.name.clone());

                write!(f, "{}", result)
            } 
        }

        impl EnveloppeCellFunctions for Rc<RefCell<Enveloppe>> {
            fn create(name: String) -> Self {
                Rc::new(RefCell::new(
                    Enveloppe {
                    children: Vec::new(),
                    is_open: true,
                    name,
                }))
            }

            fn close(&self) {
                self.borrow_mut().is_open = false;
            }

            fn displayable(&self) -> EnveloppeCellWrapper {
                EnveloppeCellWrapper{value: Rc::clone(&self)}
            }

            fn add_fragment(&self, fragment: Fragment) -> Result<(), String> {
                if !self.borrow().is_open {
                   return Err(String::from("Can't add to a closed enveloppe."));
                }

                let mut last_open = Rc::clone(self);

                loop {
                    let last_open_clone: Rc<RefCell<Enveloppe>>;
                    let last_open_env: Ref<Enveloppe>;
                    last_open_clone = Rc::clone(&last_open);
                    last_open_env = last_open_clone.borrow();

                    let open_child = last_open_env.children.iter().find(|&nested| {
                        match nested {
                            NestedContent::Other(_) => false,
                            NestedContent::Enveloppe(ref envlp) => envlp.borrow().is_open,
                        }
                    });

                    match open_child {
                        None => break,
                        Some(nested) =>  {
                            if let NestedContent::Enveloppe(ref envlp) = nested {
                                last_open = Rc::clone(envlp);
                            } else {
                                panic!("Enveloppe.add_fragment: if an open enveloppe was found, a test on it being a NestedContent::Enveloppe shouldn't fail.");
                            }
                        }
                    }
                };

                match fragment {
                    OpenMark(name) => last_open.borrow_mut().children.push(NestedContent::Enveloppe(Rc::<RefCell<Enveloppe>>::create(name.to_string()))),
                    Other(content) => last_open.borrow_mut().children.push(NestedContent::Other(content.to_string())),
                    CloseMark => last_open.close(),
                }

                Ok(())
            }
        }

        #[test]
        fn nom_open_close_same_name() {
            let name_stack: RefCell<Vec<String>> = RefCell::new(Vec::new());
            
            let mut open_mark_first = make_open_mark!(name_stack, true);
            let open_mark_nested = make_open_mark!(name_stack, false);

            assert_eq!(Ok(("12", OpenMark("a"))), open_mark_first("<a12"));

            let mut close_mark_last = make_close_mark!(name_stack, true);
            let close_mark_nested = make_close_mark!(name_stack, false);

            assert_eq!(Ok(("zz", CloseMark)), close_mark_last(">azz"));

            let mut enveloppe = map(
                tuple((
                    open_mark_first,
                    many0(alt((
                        map(is_not("<>"), |other| Fragment::Other(other)),
                        open_mark_nested,
                        close_mark_nested,
                    ))),
                    close_mark_last
                )),
                |tp|
                {
                    let result: Rc<RefCell<Enveloppe>>;

                    if let OpenMark(name) = tp.0 {
                        result = Rc::<RefCell<Enveloppe>>::create(name.to_string());
                    } else {
                        panic!("The output of open_mark_first should be a Fragment::OpenMark.");
                    }

                    for fragment in tp.1 {
                        result.add_fragment(fragment);
                    }

                    result.close();

                    result
                }
            );

            name_stack.borrow_mut().clear();
            assert_eq!(
                "<x_-_-_>x".to_string(),
                enveloppe("<x_-_-_>x").unwrap().1.displayable().to_string()
            );

            name_stack.borrow_mut().clear();
            assert_eq!(
                "<x_<x_>x>x".to_string(),
                enveloppe("<x_<x_>x>x").unwrap().1.displayable().to_string()
            );

            name_stack.borrow_mut().clear();
            assert_eq!(
                "<x>x".to_string(),
                enveloppe("<x>x").unwrap().1.displayable().to_string()
            );

            name_stack.borrow_mut().clear();
            assert_eq!(
                "<x_<y->y_>x".to_string(),
                enveloppe("<x_<y->y_>x").unwrap().1.displayable().to_string()
            );

            name_stack.borrow_mut().clear();
            assert_eq!(
                "<x_<y->y_>x".to_string(),
                enveloppe("<x_<y->y_>x---<a--->a").unwrap().1.displayable().to_string()
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
