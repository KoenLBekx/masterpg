use std::cmp::min;
use std::collections::HashMap;
use std::ffi::OsStr;
use std::mem::discriminant;
use std::str::FromStr;
use nom::{
    branch::alt,
    bytes::complete::{is_a, is_not, tag, take},
    character::complete::char,
    combinator::{map, opt, peek, recognize, verify},
    Err,
    error::{Error, ErrorKind},
    IResult,
    multi::{many0, many1, separated_list0},
    Parser,
    sequence::{delimited, pair, preceded, terminated}
};
use string_io_and_mock::TextIOHandler;
use tree_by_path::Node;

#[derive(PartialEq)]
#[derive(Debug)]
#[derive(Clone)]
enum Operand {
    PlaceHolder(String),
    Value(f64),
}
impl Operand {
    pub fn new(source: String) -> Self {
        let parse_result = f64::from_str(source.replace(",", ".").as_str());

        match parse_result {
            Ok(num) => Operand::Value(num),
            Err(_) => Operand::PlaceHolder(source.to_string()),
        }
    }

    pub fn new_value(value: f64) -> Self {
        Operand::Value(value)
    }
}

#[derive(PartialEq)]
#[derive(Debug)]
#[derive(Clone)]
struct Calculation {
    name: String,
    operator: String,
    operands: Vec<Operand>,
}
impl Calculation {
    fn new(words: Vec<String>) -> Self {
        let mut opds = Vec::<Operand>::new();

        for i in 2..words.len() {
            opds.push(Operand::new(words[i].clone()));
        }

        Calculation {
            name: words[0].clone(),
            operator: words[1].clone(),
            operands: opds,
        }
    }

    fn resolve_operand(&mut self, op_index: usize, value: f64) {
        self.operands[op_index] = Operand::new_value(value);
    }
}

#[derive(PartialEq)]
#[derive(Debug)]
#[derive(Clone)]
enum NestedPageContent {
    Main,
    Output(String),
    Master(String),
    Actual(String),
    Calc(Calculation),
    PlaceHolder(String),
    Other(String),
    Resolved(String),
}
impl NestedPageContent {
    fn new(words: &Vec<String>) -> Result<NestedPageContent, String> {
        if words.len() < 1 {
            Err("fn NestedPageContent::new expects at least one element in its words parameter.".to_string())
        } else {
            let tag_type = words[0].as_str();

            let required_words: usize = match tag_type {
                "main" => 1,
                "output" | "master" | "actual" | "placeholder" | "other" | "resolved" => 2,
                "calc" => 5,
                _ => return Err(format!("Invalid masterpage tag found : {}.", tag_type).to_string()),
            };

            if words.len() < required_words {
                return Err(format!("Insufficient attributes in masterpage tag {}", tag_type).to_string());
            }

            let new_content = match tag_type {
                "main" => NestedPageContent::Main,
                "output" => NestedPageContent::Output(words[1].clone()),
                "master" => NestedPageContent::Master(words[1].clone()),
                "actual" => NestedPageContent::Actual(words[1].clone()),
                "placeholder" => NestedPageContent::PlaceHolder(words[1].clone()),
                "other" => NestedPageContent::Other(words[1].clone()),
                "resolved" => NestedPageContent::Resolved(words[1].clone()),
                "calc" => NestedPageContent::Calc(Calculation::new(vec![words[1].clone(), words[2].clone(), words[3].clone(), words[4].clone()])),
                _ => return Err(format!("Invalid masterpage tag found : {}.", tag_type).to_string()),
            };

            Ok(new_content)
        }
    }
}

impl std::fmt::Display for NestedPageContent {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let output = match self {
            NestedPageContent::Main => "Main".to_string(),
            NestedPageContent::Output(ref word) => format!("Output({word})"),
            NestedPageContent::Master(ref word) => format!("Master({word})"),
            NestedPageContent::Actual(ref word) => format!("Actual({word})"),
            NestedPageContent::PlaceHolder(ref word) => format!("PlaceHolder({word})"),
            NestedPageContent::Other(ref text) => format!("Other({})", &text[0..min(10, text.len())]),
            NestedPageContent::Calc(_) => "Calc".to_string(),
            NestedPageContent::Resolved(ref word) => format!("Resolved({word})"),
        };

        write!(f, "{}", output)
    }
}

#[derive(PartialEq)]
#[derive(Debug)]
enum FlatPageContent {
    SelfContainedMPTag(Vec<String>),
    OpeningMPTag(Vec<String>),
    ClosingMPTag(String),
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

fn master_page_tag_opening_children(input: &str) -> IResult<&str, FlatPageContent> {
    match master_page_tag_opening_content(input, false) {
        Err(err) => Err(err),
        Ok((rest, tag_strings)) => Ok((rest, FlatPageContent::OpeningMPTag(tag_strings))),
    }
}

fn master_page_tag_self_contained(input: &str) -> IResult<&str, FlatPageContent> {
    match master_page_tag_opening_content(input, true) {
        Err(err) => Err(err),
        Ok((rest, tag_strings)) => Ok((rest, FlatPageContent::SelfContainedMPTag(tag_strings))),
    }
}

fn master_page_closing_tag(input: &str) -> IResult<&str, FlatPageContent> {
    map(
        delimited(
            terminated(tag("</+"), opt(whitespace)),
            is_not(" \t\n\r/><"),
            preceded(opt(whitespace), tag(">"))
        ),
        |found| FlatPageContent::ClosingMPTag(String::from(found))
    )(input)
}

fn other_flat_content(input: &str) -> IResult<&str, FlatPageContent> {
    map(
        many1(
            alt((
                is_not("<"),
                terminated(
                    alt((
                        terminated(tag("<"), peek(is_not("/"))),
                        tag("</")
                    )),
                    peek(is_not("+"))
                ),
            ))
        ),
        |list: Vec<&str>|
        {
            let mut result_str = String::new();

            for elem in list {
                result_str.push_str(elem);
            }

            FlatPageContent::Other(result_str)
        }
    )(input)
}

fn read_flat_content(input: &str) -> Result<Vec<FlatPageContent>, String> {
    let input_len = input.len();

    let nom_result = many0(
        alt((
            other_flat_content,
            master_page_tag_self_contained,
            master_page_tag_opening_children,
            master_page_closing_tag,
        ))
    )(input);

    match nom_result {
        Err(_) => Err("Malformed input string.".to_string()),
        Ok((rest, contents)) =>  {
            match rest.len() {
                0 => Ok(contents),
                rest_len =>  {
                    let err_start = input_len - rest_len + 1;
                    
                    Err(format!("The master page tag starting around character {} is malformed.", err_start))
                },
            }
        },
    }
}

fn add_content_to_root(root: &mut Node<NestedPageContent>, path: &Vec<usize>, content: NestedPageContent) -> Result<Vec<usize>, String> {
    match root.add_cargo_under(path, content) {
        Ok(added_path) => Ok(added_path),
        Err((err, _)) => Err(format!("{:?}", err)),
    }
}

fn borrow_content_from_root<'r, 'p>(root: &'r Node<NestedPageContent>, path: &'p Vec<usize>) -> Result<&'r NestedPageContent, String> {
    match root.borrow_cargo(path) {
        Ok(cargo) => Ok(cargo),
        Err(err) => Err(format!("{:?}", err)), 
    }
}

fn read_nested_content(input: &str) -> Result<Node<NestedPageContent>, String> {
    match read_flat_content(input) {
        Err(err) => Err(err),
        Ok(flat_contents) => {

            /*
            #[cfg(test)]
            println!("{:#?}", &flat_contents);
            */

            let mut root = Node::new(NestedPageContent::new(&vec!["main".to_string()])?);
            let mut path = root.get_first_path();

            for flat_content in flat_contents {

                /*
                #[cfg(test)]
                {
                    println!("----------------------------------------");
                    println!("{:#?}", &root);
                }
                */

                match flat_content {
                    FlatPageContent::Other(text) => {
                        add_content_to_root(&mut root, &path, NestedPageContent::new(&vec!["other".to_string(), text])?)?;
                    },
                    FlatPageContent::SelfContainedMPTag(words) => {
                        match words[0].as_str() {
                            "actual" => return Err("An <+actual ...>...</+actual> tag should have children, and shouldn't be self-contained like <+actual .../>.".to_string()),
                            _ => (),
                        }

                        add_content_to_root(&mut root, &path, NestedPageContent::new(&words)?)?;
                    },
                    FlatPageContent::OpeningMPTag(words) => {
                        let new_cargo: NestedPageContent;

                        match words[0].as_str() {
                            "actual" => new_cargo = NestedPageContent::new(&words)?,
                            _ => return Err("Invalid non self-contained masterpage tag found.".to_string()),
                        }

                        path = add_content_to_root(&mut root, &path, new_cargo)?;
                    },
                    FlatPageContent::ClosingMPTag(word) => {
                        // Check if the the parent NestedPageContent is the same variant.
                        let expected_variant = match word.as_str() {
                            "actual" => NestedPageContent::Actual("dummy".to_string()),
                            _ => return Err("Invalid or unknown closing masterpage tag found.".to_string()),
                        };

                        let parent = borrow_content_from_root(&root, &path)?;

                        /*
                        #[cfg(test)]
                        println!("========== Parent node's cargo : {:?}", &parent);
                        */

                        if discriminant(&expected_variant) != discriminant(parent) {
                            return Err("Incorrectly paired opening and closing masterpage tags found.".to_string());
                        }

                        path.pop();
                    },
                }
            }

            // The path should again point to the main content node.
            if &NestedPageContent::Main != borrow_content_from_root(&root, &path)? {
                Err("Unclosed masterpage tag found.".to_string())
            } else {
                Ok(root)
            }
        },
    }
}

fn read_text_from_handler<Tioh>(handler: &Tioh, file_path: &OsStr) -> Result<String, String>
where Tioh: TextIOHandler {
    match handler.read_text(file_path) {
        Ok(text) => Ok(text),
        Err(io_err) => Err(format!("{:?}", io_err)),
    }
}

fn resolve_masters_in_tree<Tioh>(io_handler: &Tioh, mut content_tree: Node<NestedPageContent>) -> Result<Node<NestedPageContent>, String>
where Tioh: TextIOHandler {
    // Resolve all master tags by replacing them with resolved tags containing their referenced file's contents.
    loop {
        // Get all master files as trees.
        let master_trees_result = content_tree.traverse(
            Ok(Vec::<Node<NestedPageContent>>::new()),
            |trees_result: &mut Result<Vec<Node<NestedPageContent>>, String>, node, _path|{
                let mut do_go_on = true;

                match node.cargo {
                    NestedPageContent::Master(ref name) => {
                        match read_text_from_handler(io_handler, &OsStr::new(name)) {
                            Ok(master_text) => {
                                match read_nested_content(master_text.as_str()) {
                                    Ok(master_tree) =>  {
                                        match trees_result {
                                            Ok(ref mut trees) => {
                                                trees.push(master_tree);

                                                let resolved = NestedPageContent::new(&vec!["resolved".to_string(), name.clone()]);
                                                match resolved {
                                                    Ok(rslv) => node.cargo = rslv,
                                                    Err(err) => {
                                                        do_go_on = false;
                                                        *trees_result = Err(err)
                                                    },
                                                }
                                            },
                                            // Should never occur, but we'll cover this anyway.
                                            Err(_) => do_go_on = false,
                                        }
                                    },
                                    Err(err) => {
                                        do_go_on = false;
                                        *trees_result = Err(err)
                                    },
                                }
                            },
                            Err(err) => {
                                do_go_on = false;
                                *trees_result = Err(err)
                            },
                        }
                    },
                    _ => (),
                }

                do_go_on
            }
        );

        match master_trees_result {
            Err(err) => return Err(err),
            Ok(master_trees) => {
                if master_trees.len() == 0 {
                    break;
                }

                // Prepend all found master trees to the content tree's children.
                for master_tree in master_trees {
                    match content_tree.add_node_before(&vec![0], master_tree) {
                        Ok(_) => (),
                        Err(path_error) => return Err(format!("Programming error in fn resolve_masters_in_tree : {:?}", path_error)),
                    }
                }
            }
        }
    }

    Ok(content_tree)
}

fn find_recursion_in_content_tree(content_tree: &mut Node<NestedPageContent>) -> Vec<String> {
    //! Check if NestedPageContent::Actual and Calc tags have no recursion by having a placeholder or
    //! calc tag with their own name.

    // Get the paths to all the Actual and Calc nodes.
    let parent_paths_and_names = content_tree.traverse(
        Vec::<(Vec<usize>, String)>::new(),
        |paths, node, node_path| {
            match node.cargo {
                NestedPageContent::Actual(ref word) => paths.push((node_path.clone(), word.clone())),
                NestedPageContent::Calc(ref calculation) => paths.push((node_path.clone(), calculation.name.clone())),
                _ => (),
            }

            true
        }
    );

    // For each found parent path, check if its children contain a PlaceHolder or Actual element with the
    // same name, or a Calc element having the same name as operand or own name.
    let mut recursives = Vec::<String>::new();

    for parent_path_and_name in parent_paths_and_names {
        let parent_name = parent_path_and_name.1;

        let parent_node = content_tree.borrow_mut_node(&parent_path_and_name.0)
            .expect("Function find_recursion_in_content_tree couldn't borrow a container node from its tree, but should have been able to.");

        let has_recursion = parent_node.traverse(
            false,
            |accum, node, path| {
                if path.len() > 0 {
                    match node.cargo {
                        NestedPageContent::PlaceHolder(ref word) => *accum = *word == parent_name,
                        NestedPageContent::Calc(ref calculation) => {
                            for opd in &calculation.operands {
                                if let Operand::PlaceHolder(ref name) = opd {
                                    *accum = *accum || (*name == parent_name);
                                }
                            }
                        },
                        _ => (),
                    }
                }
                
                !*accum
            }
        );

        if has_recursion {
            (&mut recursives).push(parent_name.clone());
        }
    }

    recursives
}

fn find_output_name_in_content_tree(content_tree: &mut Node<NestedPageContent>) -> Result<String, String> {
    content_tree.traverse(
        Err("No output name found.".to_string()),
        |result, node, _path| {
            match node.cargo {
                NestedPageContent::Output(ref name) => {
                    *result = Ok(name.clone());
                    false
                },
                _ => true,
            }
        }
    )
}

fn resolve_references_in_content_tree(content_tree: &mut Node<NestedPageContent>) -> Result<usize, String> {
    let mut actuals = HashMap::<String, Node<NestedPageContent>>::new();
    let mut resolved_count = 0usize;
    let mut unresolved_count = 0usize;
    let mut passes = 0usize;

    loop {
        passes += 1;

        // Add clones of already found actuals to hashmap.
        actuals = content_tree.traverse(
            actuals,
            |acts, node, path| {
                match node.cargo {
                    NestedPageContent::Actual(ref name) => {
                        acts.entry(name.clone()).or_insert((*node).clone());
                    },
                    _ => (),
                }

                true
            }
        );

        // Resolve placeholders and calc operands that can already be resolved.
        (unresolved_count, resolved_count) = content_tree.traverse(
            (unresolved_count, resolved_count),
            |counts, node, path| {
                match node.cargo {
                    NestedPageContent::PlaceHolder(ref name) => {
                        match actuals.get(name) {
                            None => counts.0 += 1,
                            Some(ref actual_node) => {
                                counts.1 += 1;
                                node.cargo = NestedPageContent::Resolved(name.clone());

                                // Add cloned children from the actual node to the current node.
                                for child in &actual_node.children {
                                    node.children.push(child.clone());
                                }
                            },
                        }
                    },
                    NestedPageContent::Calc(ref mut calculation) => {
                        // Check if any the calculation's operands can be resolved.
                        // If so, resolve them.
                        // If both can be resolved, replace the NestedPageContent::Calc cargo with
                        // an NestedPageContent::Resolved cargo.
                        
                        // TODO : a Calculation should have a vector of operands.
                        /*
                        match calculation.operand1 {
                            Operand::PlaceHolder(ref name) => {
                                match actuals.get(name) {
                                    None => counts.0 += 1,
                                    Some(ref actual_node) => {
                                        // Read the content of the first Other child.
                                        let num_text = actual_node.traverse(
                                            String::new(),
                                            |accum, nd, _path| {
                                                match nd.cargo {
                                                    NestedPageContent::Other(txt) => *accum = txt.clone(),
                                                    _ => (),
                                                }
                                            }

                                            accum.len() > 0
                                        );

                                        if num_text.len() > 0 {
                                            let opd = Operand::new(num_text);
                                            match opd {
                                                Operand::Value(_) => {
                                                    calculation.operand1 = opd;
                                                    counts.1 += 1;
                                                },
                                                _ => counts.0 += 1;

                                            }
                                        }
                                    },
                                }
                            },
                            _ => (),
                        }
                        */
                    },
                    _ => (),
                }

                true
            }
        );

        if (unresolved_count == 0) || (resolved_count == 0) {
            break;
        }
    }

    if unresolved_count == 0 {
        Ok(passes)
    } else {
        Err("Unresolvable placeholder tags or calculation operands found.".to_string())
    }
}

/// io_handler is mutable, for the result is written back to it.
pub fn compose<Tioh>(file_path:&OsStr, io_handler: &mut Tioh) -> Result<(), String>
where Tioh: TextIOHandler {
    let all_texts:String = read_text_from_handler(io_handler, file_path)?;
    let mut content_tree = read_nested_content(all_texts.as_str())?;
    
    content_tree = match resolve_masters_in_tree(io_handler, content_tree) {
        Err(err) => return Err(err),
        Ok(tree) => tree,
    };

    let recursions = find_recursion_in_content_tree(&mut content_tree);
    if recursions.len() > 0 {
        return Err(
            format!("{} {} {:?}.",
                "Recursion found in <+placeholder> or <+calc.../>",
                "tags referring to",
                recursions
            ).replace("[", "").replace("]","")
        );
    }

    let output_name = match find_output_name_in_content_tree(&mut content_tree) {
        Ok(name) => name,
        Err(err) => return Err(err),
    };

    // TODO : resolve all calculations and placeholders in the order they are found.
    // TODO : convert the content_tree to a flat content string.
    // TODO : write flat content string back to io_handler using found output name.
    
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use string_io_and_mock::MockTextHandler;
    use tree_by_path::Node;

    mod utils {
        use crate::NestedPageContent;
        use tree_by_path::Node;

        pub(crate) fn content_tree_to_string(content_tree: &mut Node<NestedPageContent>) -> String {
            let mut repr = content_tree.traverse(
                (String::new(), 0usize), 
                |accum, node, path|{
                    let path_len = path.len();

                    if path_len < accum.1 {
                        for _ in 0..(accum.1 - path_len){
                            (*accum).0.push_str("]");
                        }
                    }

                    if path_len > 0 {
                        if path[path_len - 1] > 0 {
                            (*accum).0.push_str(" ");
                        } else {
                            (*accum).0.push_str("[");
                        }
                    }

                    (*accum).0.push_str(node.cargo.to_string().as_str());
                    (*accum).1 = path_len;

                    true
                }
            );

            for _ in 0..repr.1 {
                repr.0.push_str("]");
            }

            repr.0
        }
    }

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
            Ok(("", FlatPageContent::ClosingMPTag("actual".to_string()))),
            master_page_closing_tag("</+actual>")
        );
    }

    #[test]
    fn master_page_closing_tag_spaces() {
        assert_eq!(
            Ok(("", FlatPageContent::ClosingMPTag("actual".to_string()))),
            master_page_closing_tag("</+ \t actual >")
        );
    }

    #[test]
    fn master_page_closing_tag_text_after() {
        assert_eq!(
            Ok(("Hello everybody, ...", FlatPageContent::ClosingMPTag("actual".to_string()))),
            master_page_closing_tag("</+ \t actual >Hello everybody, ...")
        );
    }

    #[test]
    fn master_page_closing_tag_non_greedy() {
        assert_eq!(
            Ok(("ity>", FlatPageContent::ClosingMPTag("actual".to_string()))),
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

    #[test]
    fn mpt_opening_children_fails_on_no_tag() {
        let result = master_page_tag_opening_children("abc def ghi blabla");
        assert!(result.is_err());
    }

    #[test]
    fn mpt_opening_children_fails_on_html_tag() {
        let result = master_page_tag_opening_children("<abc def ghi>blabla</abc>");
        assert!(result.is_err());
    }

    #[test]
    fn mpt_opening_children_fails_on_unclosed() {
        let result = master_page_tag_opening_children("<+abc def ghi blabla");
        assert!(result.is_err());
    }

    #[test]
    fn mpt_opening_children_fails_on_self_contained() {
        let result = master_page_tag_opening_children("<+abc def ghi/>blabla");
        assert!(result.is_err());
    }

    #[test]
    fn mpt_opening_children() {
        let result = master_page_tag_opening_children("<+abc def ghi>blabla</+abc>");
        assert!(result.is_ok());
        assert_eq!(Ok(("blabla</+abc>", FlatPageContent::OpeningMPTag(vec!["abc".to_string(), "def".to_string(), "ghi".to_string()]))), result);
    }

    #[test]
    fn mpt_self_contained_fails_on_no_tag() {
        let result = master_page_tag_self_contained("abc def ghi blabla");
        assert!(result.is_err());
    }

    #[test]
    fn mpt_self_contained_fails_on_html_tag() {
        let result = master_page_tag_self_contained("<abc def ghi/>blabla");
        assert!(result.is_err());
    }

    #[test]
    fn mpt_self_contained_fails_on_unclosed() {
        let result = master_page_tag_self_contained("<+abc def ghi blabla");
        assert!(result.is_err());
    }

    #[test]
    fn mpt_self_contained_fails_on_opening() {
        let result = master_page_tag_self_contained("<+abc def ghi>blabla</+abc>");
        assert!(result.is_err());
    }

    #[test]
    fn mpt_self_contained() {
        let result = master_page_tag_self_contained("<+abc / def ghi/>blabla");
        assert!(result.is_ok());
        assert_eq!(Ok(("blabla", FlatPageContent::SelfContainedMPTag(vec!["abc".to_string(), "/".to_string(), "def".to_string(), "ghi".to_string()]))), result);
    }

    #[test]
    fn mpt_other_empty() {
        let result = other_flat_content("");
        assert!(result.is_err());
    }

    #[test]
    fn mpt_other_no_tags() {
        let result = other_flat_content("I spied Willy McIntosh an hour before the dawning");
        assert!(result.is_ok());

        assert_eq!(
            Ok(("", FlatPageContent::Other("I spied Willy McIntosh an hour before the dawning".to_string()))),
            result
        );
    }

    #[test]
    fn mpt_other_only_html_tags() {
        let result = other_flat_content("<div class=\"verse\">As I came down by Fiddichside</div>");
        assert!(result.is_ok());

        assert_eq!(
            Ok(("", FlatPageContent::Other("<div class=\"verse\">As I came down by Fiddichside</div>".to_string()))),
            result
        );
    }

    #[test]
    fn mpt_other_followed_by_self_contained_master_page_tag() {
        let result = other_flat_content("<div class=\"verse\">As I came down by Fiddichside</div><+placeholder menu/>");
        assert!(result.is_ok());

        assert_eq!(
            Ok((
                "<+placeholder menu/>",
                FlatPageContent::Other("<div class=\"verse\">As I came down by Fiddichside</div>".to_string())
            )),
            result
        );
    }

    #[test]
    fn mpt_other_followed_by_closing_master_page_tag() {
        let result = other_flat_content("<div class=\"verse\">As I came down by Fiddichside</div></+actual>");
        assert!(result.is_ok());

        assert_eq!(
            Ok((
                "</+actual>",
                FlatPageContent::Other("<div class=\"verse\">As I came down by Fiddichside</div>".to_string())
            )),
            result
        );
    }

    #[test]
    fn read_flat_content_empty() {
        let result = read_flat_content("");
        assert!(result.is_ok());
        assert_eq!(Ok(Vec::<FlatPageContent>::new()), result);
    }

    #[test]
    fn read_flat_content_only_other() {
        let test_content = "<!doctype html><html><head></head><body><h1>Test page</h1></body></html>";
        let result = read_flat_content(test_content.clone());
        assert!(result.is_ok());
        let contents = result.unwrap();
        assert_eq!(vec![FlatPageContent::Other(test_content.to_string())], contents);
    }

    #[test]
    fn read_flat_content_mixed() {
        let test_content_string = "
<+master general.mpm/>
<+output testPage.htm/>
<!doctype html><html><head></head>
<+actual body>
<body><h1>Test page</h1></body>
</+actual>
<+placeholder body/>
</html>"
.replace("\n", "");

        let test_content = test_content_string.as_str();

        let result = read_flat_content(test_content.clone());
        assert!(result.is_ok());
        let contents = result.unwrap();
        assert_eq!(8, contents.len());

        assert_eq!(
            vec![
                FlatPageContent::SelfContainedMPTag(vec!["master".to_string(), "general.mpm".to_string()]),
                FlatPageContent::SelfContainedMPTag(vec!["output".to_string(), "testPage.htm".to_string()]),
                FlatPageContent::Other("<!doctype html><html><head></head>".to_string()),
                FlatPageContent::OpeningMPTag(vec!["actual".to_string(), "body".to_string()]),
                FlatPageContent::Other("<body><h1>Test page</h1></body>".to_string()),
                FlatPageContent::ClosingMPTag("actual".to_string()),
                FlatPageContent::SelfContainedMPTag(vec!["placeholder".to_string(), "body".to_string()]),
                FlatPageContent::Other("</html>".to_string())
            ],
            contents
        );
    }

    #[test]
    fn read_flat_content_malformed() {
        let test_content_string = "
<+master general.mpm/>
<+output testPage.htm/>
<!doctype html><html><head></head>
<+actual body>
<body><h1>Test page</h1></body>
</+actual>
<+placeholder body/
</html>"
.replace("\n", "");

        let test_content = test_content_string.as_str();

        let result = read_flat_content(test_content.clone());
        assert!(result.is_err());

        // Debug
        // println!("{}", result.unwrap_err());

        let error_text = result.unwrap_err();

        assert_eq!(
            "The master page tag starting around character 135 is malformed.".to_string(),
            error_text
        );
    }

    #[test]
    fn read_nested_content_empty() {
        let result = read_nested_content("");

        assert!(result.is_ok());
        assert_eq!(Node::new(NestedPageContent::Main), result.unwrap());
    }

    #[test]
    fn read_nested_content_mixed() {
        let test_content = "
<+output out.htm/>
<+master boilerplate.mpm/>
<+actual title>Introduction</+actual>
<+actual body>
Welcome to my site about <+placeholder title/>!
</+actual>
".replace("\n", "");

        let result = read_nested_content(test_content.as_str());
        assert!(result.is_ok());

        let mut root = result.unwrap();
        let repr = utils::content_tree_to_string(&mut root);

        assert_eq!(
            "Main[Output(out.htm) Master(boilerplate.mpm) Actual(title)[Other(Introducti)] Actual(body)[Other(Welcome to) PlaceHolder(title) Other(!)]]".to_string(),
            repr
        );
    }

    #[test]
    fn read_nested_content_self_contained_actual() {
        let test_content = "
<+output out.htm/>
<+master boilerplate.mpm/>
<+actual title/>Introduction</+actual>
".replace("\n", "");

        let result = read_nested_content(test_content.as_str());
        assert!(result.is_err());
    }

    #[test]
    fn read_nested_content_self_contained_unknown() {

        let test_content = "
<+output out.htm/>
<+master boilerplate.mpm/>
<+unknown title/>
".replace("\n", "");

        let result = read_nested_content(test_content.as_str());
        assert!(result.is_err());
    }

    #[test]
    fn read_nested_content_opening_unknown() {

        let test_content = "
<+output out.htm/>
<+master boilerplate.mpm/>
<+unknown title>Introduction</+unknown>
".replace("\n", "");

        let result = read_nested_content(test_content.as_str());
        assert!(result.is_err());
    }

    #[test]
    fn read_nested_content_mismatched_closing() {

        let test_content = "
<+output out.htm/>
<+master boilerplate.mpm/>
<+actual title>Introduction</+master>
".replace("\n", "");

        let result = read_nested_content(test_content.as_str());
        assert!(result.is_err());
    }

    #[test]
    fn read_nested_content_redundant_closing() {

        let test_content = "
<+output out.htm/>
<+master boilerplate.mpm/>
<+actual title>Introduction</+actual>
Some content
</+actual>
".replace("\n", "");

        let result = read_nested_content(test_content.as_str());
        assert!(result.is_err());
    }

    #[test]
    fn read_nested_content_unclosed_actual() {

        let test_content = "
<+output out.htm/>
<+actual title>Introduction
<+master boilerplate.mpm/>
".replace("\n", "");

        let result = read_nested_content(test_content.as_str());
        assert!(result.is_err());
    }

    #[test]
    fn operand_new_int() {
        let source = "17";
        let opd = Operand::new(source.to_string());

        assert_eq!(Operand::Value(17f64), opd);
    }

    #[test]
    fn operand_new_negative() {
        let source = "-104";
        let opd = Operand::new(source.to_string());

        assert_eq!(Operand::Value(-104f64), opd);
    }

    #[test]
    fn operand_new_fractal() {
        let source = "4.999";
        let opd = Operand::new(source.to_string());

        assert_eq!(Operand::Value(4.999f64), opd);
    }

    #[test]
    fn operand_new_comma() {
        let source = "-43,4";
        let opd = Operand::new(source.to_string());

        assert_eq!(Operand::Value(-43.4f64), opd);
    }

    #[test]
    fn operand_new_placeholder() {
        let source = "text_width";
        let opd = Operand::new(source.to_string());

        assert_eq!(Operand::PlaceHolder("text_width".to_string()), opd);
    }

    #[test]
    fn resolve_markers() {
        let main_text = "
<+output test.htm/>
<+master page.mpx/>
<+actual chapter>
Just some blahblah.
</+actual>
<+actual title>
Blahblah
</+actual>
".replace("\n", "");

        let page_intermediate = "
<+master constants.mpm/>
<!doctype html/>
<html>
<head>
<title><+placeholder title/></title>
</head>
<body>
<+placeholder generalTitle/>
<+placeholder chapter/>
</body>
</html>
".replace("\n", "");

        let page_constants = "
<+actual generalTitle>Masterpg test site</+actual>
".replace("\n", "");

        let content_tree = read_nested_content(main_text.as_str()).unwrap();

        let mut text_handler = MockTextHandler::new();
        text_handler.write_text(OsStr::new("page.mpx"), page_intermediate).unwrap();
        text_handler.write_text(OsStr::new("constants.mpm"), page_constants).unwrap();

        let result = resolve_masters_in_tree(&text_handler, content_tree);
        assert!(result.is_ok());
        let mut result_tree = result.unwrap();
        let result_tree_flat = utils::content_tree_to_string(&mut result_tree);

        // Debug 
        println!("{}", &result_tree_flat);

        assert_eq!(
            "Main[Main[Actual(generalTitle)[Other(Masterpg t)]] Main[Resolved(constants.mpm) Other(<!doctype ) PlaceHolder(title) Other(</title></) PlaceHolder(generalTitle) PlaceHolder(chapter) Other(</body></h)] Output(test.htm) Resolved(page.mpx) Actual(chapter)[Other(Just some )] Actual(title)[Other(Blahblah)]]",
            result_tree_flat
        );
    }

    #[test]
    fn find_recursion_none() {
        let content_text = "
<+output art.htm/>
<+actual contactData>
Genario Calogero
Rambla de la Paz, 7
Taragona
</+actual>
<!doctype html/>
<etc>
".replace("\n", "");

        let mut content_tree = read_nested_content(content_text.as_str()).unwrap();
        let recursives = find_recursion_in_content_tree(&mut content_tree);

        assert_eq!(0, recursives.len());
    }

    #[test]
    fn find_recursion_placeholder() {
        let content_text = "
<+output art.htm/>
<+actual contactData>
Genario Calogero
<+placeholder contactData/>
</+actual>
<!doctype html/>
<etc>
".replace("\n", "");

        let mut content_tree = read_nested_content(content_text.as_str()).unwrap();
        let recursives = find_recursion_in_content_tree(&mut content_tree);

        assert_eq!(1, recursives.len());
        assert_eq!("contactData".to_string(), recursives[0]);
    }

    #[test]
    fn find_recursion_calc_operand1() {
        let content_text = "
<+output art.htm/>
<+actual contactData>
Genario Calogero
<+calc crazy + contactData 19/>
</+actual>
<!doctype html/>
<etc>
".replace("\n", "");

        let mut content_tree = read_nested_content(content_text.as_str()).unwrap();
        let recursives = find_recursion_in_content_tree(&mut content_tree);

        assert_eq!(1, recursives.len());
        assert_eq!("contactData".to_string(), recursives[0]);
    }

    #[test]
    fn find_recursion_calc_operand2() {
        let content_text = "
<+output art.htm/>
<+actual contactData>
Genario Calogero
<+calc crazy + 39 contactData/>
</+actual>
<!doctype html/>
<etc>
".replace("\n", "");

        let mut content_tree = read_nested_content(content_text.as_str()).unwrap();
        let recursives = find_recursion_in_content_tree(&mut content_tree);

        assert_eq!(1, recursives.len());
        assert_eq!("contactData".to_string(), recursives[0]);
    }

    #[test]
    fn find_recursion_two() {
        let content_text = "
<+output art.htm/>
<+actual contactData>
Genario Calogero
<+calc crazy + 39 contactData/>
</+actual>
<+actual webData>
e-mail: gencalogero@famous_server.com<br />
home: <+placeholder webData/>
</+actual>
<!doctype html/>
<etc>
".replace("\n", "");

        let mut content_tree = read_nested_content(content_text.as_str()).unwrap();
        let recursives = find_recursion_in_content_tree(&mut content_tree);

        assert_eq!(2, recursives.len());
        assert_eq!("contactData".to_string(), recursives[0]);
        assert_eq!("webData".to_string(), recursives[1]);
    }

    #[test]
    fn compose_has_recursion() {
        let content_text = "
<+output art.htm/>
<+actual contactData>
Genario Calogero
<+calc crazy + 39 contactData/>
</+actual>
<+actual webData>
e-mail: gencalogero@famous_server.com<br />
home: <+placeholder webData/>
</+actual>
<!doctype html/>
<etc>
".replace("\n", "");


        let mut text_handler = MockTextHandler::new();
        let page_file_name = OsStr::new("page.mpx");
        text_handler.write_text(&page_file_name, content_text).unwrap();

        let result = compose(&page_file_name, &mut text_handler);
        assert!(result.is_err());

        // Debug
        // println!("Compose error : {}", result.unwrap_err());

        assert_eq!(
            "Recursion found in <+placeholder> or <+calc.../> tags referring to \"contactData\", \"webData\".",
            result.unwrap_err()
        );

        let read_output_result = text_handler.read_text(&OsStr::new("art.htm"));
        assert!(read_output_result.is_err());
        assert_eq!(std::io::ErrorKind::NotFound, read_output_result.unwrap_err().kind());
    }

    #[test]
    fn find_output_missing() {
        let content_text = "
<+master general.mpm/>
<+actual contactData>
Genario Calogero
Rambla de la Paz, 7
Taragona
</+actual>
<!doctype html/>
<etc>
".replace("\n", "");

        let mut content_tree = read_nested_content(content_text.as_str()).unwrap();
        let result = find_output_name_in_content_tree(&mut content_tree);
        assert!(result.is_err());
    }

    #[test]
    fn find_output() {
        let content_text = "
<+master general.mpm/>
<+actual contactData>
Genario Calogero
Rambla de la Paz, 7
Taragona
</+actual>
<+output MySite.htm/>
<!doctype html/>
<etc>
".replace("\n", "");

        let mut content_tree = read_nested_content(content_text.as_str()).unwrap();
        let result = find_output_name_in_content_tree(&mut content_tree);
        assert!(result.is_ok());
        assert_eq!("MySite.htm".to_string(), result.unwrap());
    }

    #[test]
    fn compose_missing_output_name() {
        let content_text = "
<+actual contactData>
Genario Calogero
</+actual>
<+actual webData>
e-mail: gencalogero@famous_server.com<br />
</+actual>
<!doctype html/>
<etc>
".replace("\n", "");


        let mut text_handler = MockTextHandler::new();
        let page_file_name = OsStr::new("page.mpx");
        text_handler.write_text(&page_file_name, content_text).unwrap();

        let result = compose(&page_file_name, &mut text_handler);
        assert!(result.is_err());

        assert_eq!(
            "No output name found.".to_string(),
            result.unwrap_err()
        );

        let read_output_result = text_handler.read_text(&OsStr::new("art.htm"));
        assert!(read_output_result.is_err());
        assert_eq!(std::io::ErrorKind::NotFound, read_output_result.unwrap_err().kind());

    }

    #[test]
    fn resolve_references_placeholders() {
        let content_source_str = "
<+actual title>Test page</+actual>
<!doctype html>
<html>
<head>
<title><+placeholder title/></title>
</head>
<body>
<+placeholder page_content/>
</body>
</html>
<+actual page_content>
This is a test page.
</+actual>
".replace("\n", "");

        let content_source = content_source_str.as_str();

        let mut content_tree = read_nested_content(content_source).unwrap();
        let result = resolve_references_in_content_tree(&mut content_tree);
        assert!(result.is_ok());
        assert_eq!(1, result.unwrap());
        
        // Debug
        // println!("{}", utils::content_tree_to_string(&mut content_tree));

        assert_eq!(
            "Main[Actual(title)[Other(Test page)] Other(<!doctype ) Resolved(title)[Other(Test page)] Other(</title></) Resolved(page_content)[Other(This is a )] Other(</body></h) Actual(page_content)[Other(This is a )]]".to_string(),
            utils::content_tree_to_string(&mut content_tree)
        );
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
                                _nm =>  {
                                    if $is_first && ($name_stack.borrow().len() > 0) {
                                        return false;
                                    }

                                    if (!$is_first) && ($name_stack.borrow().len() < 1) {
                                        return false;
                                    }

                                    $name_stack.borrow_mut().push(name.to_string());

                                    true
                                },
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
                    NestedContent::Other(ref the_string) => the_string.clone(),
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
            fn replace_enveloppe_having_name_with_other(&self, env_name: &str, other_content: String) -> usize;
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
                let result = format!(
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

            fn replace_enveloppe_having_name_with_other(&self, env_name: &str, other_content: String) -> usize {
                let mut replaced: usize = 0;
                let mut self_env = self.borrow_mut();

                for i in 0..self_env.children.len() {
                    replaced += match self_env.children[i] {
                        NestedContent::Other(_) => 0,
                        NestedContent::Enveloppe(ref nested_cell) => {
                            if nested_cell.borrow().name == env_name {
                                self_env.children[i] = NestedContent::Other(other_content.clone());

                                1
                            } else {
                                let nested_env = Rc::clone(nested_cell);

                                nested_env.replace_enveloppe_having_name_with_other(env_name, other_content.clone())
                            }
                        },
                    }
                }

                replaced
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
                        result.add_fragment(fragment).unwrap();
                    }

                    result.close();

                    result
                }
            );

            name_stack.borrow_mut().clear();
            let found_enveloppe = enveloppe("<x_-_-_>x").unwrap().1;

            // Ensure we can do .to_string() twice (so it's non-consuming).
            for _i in 0..2 {
                assert_eq!(
                    "<x_-_-_>x".to_string(),
                    found_enveloppe.displayable().to_string()
                );
            }

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

            name_stack.borrow_mut().clear();
            let env_cell = enveloppe("<a___>a").unwrap().1;
            let r = env_cell.replace_enveloppe_having_name_with_other("b", "replaced".to_string());
            assert_eq!(0usize, r);
            assert_eq!(
                "<a___>a".to_string(),
                env_cell.displayable().to_string()
            );

            name_stack.borrow_mut().clear();
            let env_cell = enveloppe("<a<b>b>a").unwrap().1;
            let r = env_cell.replace_enveloppe_having_name_with_other("b", "replaced".to_string());
            assert_eq!(1usize, r);
            assert_eq!(
                "<areplaced>a".to_string(),
                env_cell.displayable().to_string()
            );

            name_stack.borrow_mut().clear();
            let env_cell = enveloppe("<a_<b_>b_<c_<b_>b_>c_>a").unwrap().1;
            let r = env_cell.replace_enveloppe_having_name_with_other("b", "!".to_string());
            assert_eq!(2usize, r);
            assert_eq!(
                "<a_!_<c_!_>c_>a".to_string(),
                env_cell.displayable().to_string()
            );

            name_stack.borrow_mut().clear();
            let env_cell = enveloppe("<a_<b_>b_<c_<b_<b_>b_>b_>c_>a").unwrap().1;
            let r = env_cell.replace_enveloppe_having_name_with_other("b", "!".to_string());
            assert_eq!(2usize, r);
            assert_eq!(
                "<a_!_<c_!_>c_>a".to_string(),
                env_cell.displayable().to_string()
            );

            name_stack.borrow_mut().clear();
            let env_cell = enveloppe("<a_<b_>b_<c_<b_>b_>c_>a").unwrap().1;
            let r = env_cell.replace_enveloppe_having_name_with_other("z", "!".to_string());
            assert_eq!(0usize, r);
            assert_eq!(
                "<a_<b_>b_<c_<b_>b_>c_>a".to_string(),
                env_cell.displayable().to_string()
            );
        }
    }
}