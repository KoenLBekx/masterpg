#![doc = include_str!("../README.md")]

//{ use ...
use std::cmp::min;
use std::collections::HashMap;
use std::ffi::OsStr;
use laconic::{ExecutionOutcome, Interpreter};
use std::mem::discriminant;
use std::str::FromStr;
use std::time::SystemTime;
use nom::{
    branch::alt,
    bytes::complete::{is_a, is_not, tag},
    combinator::{map, opt, peek},
    error::ErrorKind,
    IResult,
    multi::{many0, many1},
    Parser,
    sequence::{delimited, pair, preceded, terminated}
};
use string_io_and_mock::TextIOHandler;
use tree_by_path::{Node, TraverseAction}; //}

//{ Constants
const LACONIC_SCRIPT_OPEN: char = '{';
const LACONIC_SCRIPT_CLOSE: char = '}';
//}

/// See the crate's documentation about functionality.
pub fn compose<Tioh>(file_path:&OsStr, io_handler: &mut Tioh) -> Result<Vec<String>, String>
where Tioh: TextIOHandler {
    let all_texts:String = read_text_from_handler(io_handler, file_path)?;
    let mut content_tree = read_nested_content(all_texts.as_str())?;
    
    content_tree = resolve_masters_in_tree(io_handler, content_tree)?;

    let recursions = find_recursion_in_content_tree(&mut content_tree);

    if !recursions.is_empty() {
        return Err(
            format!("{} {} {:?}.",
                "Recursion found in <+placeholder> or <+calc.../>",
                "tags referring to",
                recursions
            ).replace('[', "").replace(']',"")
        );
    }

    let output_name_string  = find_output_name_in_content_tree(&mut content_tree)?;
    let output_name = OsStr::new(&output_name_string);

    // Resolve all calculations and placeholders.
    let laconic_errors_found = resolve_references_in_content_tree(&mut content_tree)?;

    let output = content_tree_to_output_string(&mut content_tree);

    match io_handler.write_text(output_name, output) {
        Ok(_) => Ok(laconic_errors_found),
        Err(err) => Err(err.to_string())
    }
}

#[derive(PartialEq, Debug, Clone)]
enum Operand {
    PlaceHolder(String),
    Value(f64),
}
impl Operand {
    pub fn new(source: String) -> Self {
        let parse_result = f64::from_str(source.replace(',', ".").as_str());

        match parse_result {
            Ok(num) => Operand::Value(num),
            Err(_) => Operand::PlaceHolder(source.to_string()),
        }
    }

    pub fn get_value_or_default(&self, default: f64) -> f64 {
        match self {
            Operand::PlaceHolder(_) => default,
            Operand::Value(value) => *value,
        }
    }
}
impl std::fmt::Display for Operand {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let output = match self {
            Operand::PlaceHolder(ref text) => format!("Opd::P({})", text),
            Operand::Value(val) => format!("Opd::V({})", val),
        };

        write!(f, "{}", output)
    }
}

#[derive(PartialEq, Debug, Clone)]
enum CalcError {
    OutOfBounds,
    NotANumber,
    DivisionByZero,
    UnknownOperator(String),
}
impl std::fmt::Display for CalcError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let output = match self {
            CalcError::OutOfBounds => "OutOfBounds".to_string(),
            CalcError::NotANumber => "NotANumber".to_string(),
            CalcError::DivisionByZero => "DivisionByZero".to_string(),
            CalcError::UnknownOperator(opr) => format!("UnknownOperator: '{}'", opr),
        };

        write!(f, "{}", output)
    }
}

#[derive(PartialEq, Debug, Clone)]
enum CalculationValue {
    Unresolved,
    Resolved(f64),
    Invalid(CalcError),
}

#[derive(PartialEq, Debug, Clone)]
enum Operator {
    Add,
    Subtract,
    Multiply,
    Divide,
    Remainder,
    Min,
    Max,
    Abs,
    Exp,
    Sign,
    Round,
    Trunc,
    Floor,
    Ceiling,
}

#[derive(PartialEq, Debug, Clone)]
struct Calculation {
    name: String,
    operator: Operator,
    operands: Vec<Operand>,
    outcome: CalculationValue,
}
impl Calculation {
    fn new(words: Vec<String>) -> Result<Self, String> {
        let mut opds = Vec::<Operand>::new();

        for word in words.iter().skip(2) {
            opds.push(Operand::new(word.clone()));
        }

        let opr_string = words[1].to_lowercase();

        let operator = match opr_string.as_str() {
            "+" => Operator::Add,
            "-" => Operator::Subtract,
            "*" | "x" => Operator::Multiply,
            "/" | ":" | "÷" => Operator::Divide,
            "%" => Operator::Remainder,
            "min" => Operator::Min,
            "max" => Operator::Max,
            "abs" => Operator::Abs,
            "exp" | "pow" | "^" | "**" => Operator::Exp,
            "sign" => Operator::Sign,
            "round" => Operator::Round,
            "trunc" => Operator::Trunc,
            "floor" => Operator::Floor,
            "ceiling" => Operator::Ceiling,
            _ => return Err(CalcError::UnknownOperator(opr_string).to_string()),
        };

        Ok(Calculation {
            name: words[0].clone(),
            operator,
            operands: opds,
            outcome: CalculationValue::Unresolved,
        })
    }

    fn get_value(&mut self) -> CalculationValue {
        // Don't recalculate.
        match self.outcome {
            CalculationValue::Resolved(_) | CalculationValue::Invalid(_) => self.outcome.clone(),
            CalculationValue::Unresolved => {
                // Check if all operands are Operand::Value
                let mut value_count = 0usize;

                for operand in &self.operands {
                    if let &Operand::Value(_) = operand {
                        value_count += 1;
                    }
                }

                if value_count < self.operands.len() {
                    return CalculationValue::Unresolved;
                }

                let default_val: f64 = match self.operator {
                    Operator::Multiply |
                    Operator::Divide |
                    Operator::Exp => 1f64,
                    Operator::Min => f64::MAX,
                    Operator::Max => f64::MIN,
                    _ => 0f64,
                };

                let opds: Vec<f64> = self.operands.iter()
                    .map(|opd| opd.get_value_or_default(default_val)).collect();

                if self.operands.is_empty() {
                    return CalculationValue::Resolved(default_val);
                }

                let mut calc_outcome = match self.operator {
                    Operator::Abs => opds[0].abs(),
                    Operator::Sign => opds[0].signum(),
                    Operator::Round => opds[0].round(),
                    Operator::Trunc => opds[0].trunc(),
                    Operator::Floor => opds[0].floor(),
                    Operator::Ceiling => opds[0].ceil(),
                    _ => opds[0]
                };

                for &opd in &opds[1..] {
                    calc_outcome = match self.operator {
                        Operator::Add => calc_outcome + opd,
                        Operator::Subtract => calc_outcome - opd,
                        Operator::Multiply => calc_outcome * opd,
                        Operator::Divide => {
                            if opd == 0_f64 {
                                self.outcome = CalculationValue::Invalid(CalcError::DivisionByZero);
                                return self.outcome.clone();
                            }

                            calc_outcome / opd
                        },
                        Operator::Remainder => calc_outcome % opd,
                        Operator::Min => calc_outcome.min(opd),
                        Operator::Max => calc_outcome.max(opd),
                        Operator::Exp => calc_outcome.powf(opd),
                        Operator::Sign => calc_outcome / opd.signum(),
                        _ => calc_outcome,
                    };
                }

                // Matching against floats will cause a compiler error in later versions of Rust,
                // so we have to use an if-test.
                let result = if calc_outcome.is_infinite() {
                    CalculationValue::Invalid(CalcError::OutOfBounds)
                } else if calc_outcome.is_nan() {
                    CalculationValue::Invalid(CalcError::NotANumber)
                } else {
                    CalculationValue::Resolved(calc_outcome)
                };

                self.outcome = result.clone();
                result
            }
        }
    }
}
impl std::fmt::Display for Calculation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let operands = self.operands.iter()
            .fold(String::new(), |mut accum, opd| {
                let opstr = opd.to_string();
                accum.push(' ');
                accum.push_str(opstr.as_str());
                accum
            }
        );

        let output = format!("Calc({} {:?} {})", &self.name, &self.operator, operands);

        write!(f, "{}", output)
    }
}

#[derive(PartialEq, Debug, Clone)]
enum LaconicValue {
    Unresolved,
    Resolved(String),
    Invalid(String),
}

#[derive(PartialEq, Debug, Clone)]
struct LaconicScript {
    name: String,
    script: String,
    outcome: LaconicValue,
}
impl LaconicScript {
    fn new(words: Vec<String>) -> Result<Self, String> {
        if words.len() != 2 {
            #[cfg(test)]
            for (wnr, wd) in words.iter().enumerate() {
                println!("Word {wnr}: {}", wd);
            }

            return Err("Not exactly 2 attributes found after tag name in <+laconic .../> tag".to_string());
        }

        Ok(LaconicScript {
            name: words[0].clone(),
            script: words[1].clone(),
            outcome: LaconicValue::Unresolved,
        })
    }

    fn get_value(&mut self, interpreter: &mut Interpreter) -> LaconicValue {
        match interpreter.execute(self.script.clone()) {
            Ok(execution_outcome) => match execution_outcome {
                ExecutionOutcome::Empty => LaconicValue::Resolved(String::new()),
                ExecutionOutcome::Number{value: num, format_info: formatter} if !interpreter.is_quiet() => LaconicValue::Resolved(formatter.format(num)),
                ExecutionOutcome::Number{value: _num, format_info: _formatter} => LaconicValue::Resolved(String::new()),
                ExecutionOutcome::Text(txt) if !interpreter.is_quiet() => LaconicValue::Resolved(txt),
                ExecutionOutcome::Text(_txt) => LaconicValue::Resolved(String::new()),
                ExecutionOutcome::Error(script_err) => LaconicValue::Invalid(script_err.to_string()),
            },
            Err(script_err) => LaconicValue::Invalid(script_err.to_string()),
        }
    }
}
impl std::fmt::Display for LaconicScript {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let output = format!("Laconic({} {})", &self.name, &self.script);

        write!(f, "{}", output)
    }
}

#[derive(PartialEq, Debug, Clone)]
enum NestedPageContent {
    Main,
    Output(String),
    Master(String),
    Actual(String),
    Calc(Calculation),
    Laconic(LaconicScript),
    PlaceHolder(String),
    Other(String),
    Resolved(String),
    Timestamp,
    Comment,
}
impl NestedPageContent {
    fn new(words: &[String]) -> Result<NestedPageContent, String> {
        if words.is_empty() {
            Err("fn NestedPageContent::new expects at least one element in its words parameter.".to_string())
        } else {
            let tag_type_lower = words[0].to_lowercase();
            let tag_type = tag_type_lower.as_str();

            let required_words: usize = match tag_type {
                "main" | "comment" | "timestamp" => 1,
                "output" | "master" | "actual" | "placeholder" | "calc" | "other" | "resolved" | "laconic" => 2,
                _ => return Err(format!("Invalid masterpage tag found : {}.", tag_type).to_string()),
            };

            if words.len() < required_words {
                return Err(format!("Insufficient attributes in masterpage tag {}", tag_type).to_string());
            }

            // to_vec() clones the words.
            let attributes: Vec<String> = words[1..].to_vec();

            let new_content = match tag_type {
                "main" => NestedPageContent::Main,
                "output" => NestedPageContent::Output(attributes[0].clone()),
                "master" => NestedPageContent::Master(attributes[0].clone()),
                "actual" => NestedPageContent::Actual(attributes[0].clone()),
                "placeholder" => NestedPageContent::PlaceHolder(attributes[0].clone()),
                "other" => NestedPageContent::Other(attributes[0].clone()),
                "resolved" => NestedPageContent::Resolved(attributes[0].clone()),
                "calc" => NestedPageContent::Calc(Calculation::new(attributes)?),
                "laconic" => NestedPageContent::Laconic(LaconicScript::new(attributes)?),
                "timestamp" => NestedPageContent::Timestamp,
                "comment" => NestedPageContent::Comment,
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
            NestedPageContent::Calc(calculation) => calculation.to_string(),
            NestedPageContent::Laconic(laconic_script) => laconic_script.to_string(),
            NestedPageContent::Resolved(ref word) => format!("Resolved({word})"),
            NestedPageContent::Timestamp => "Timestamp".to_string(),
            NestedPageContent::Comment => "Comment".to_string(),
        };

        write!(f, "{}", output)
    }
}

#[derive(PartialEq, Debug)]
enum FlatPageContent {
    SelfContainedMPTag(Vec<String>),
    OpeningMPTag(Vec<String>),
    ClosingMPTag(String),
    Other(String),
}

fn whitespace(input: &str)-> IResult<&str, &str> {
    is_a(" \t\n\r")(input)
}

fn delimited_by_matching_multiples(opening: char, closing: char) -> impl Fn(&str) -> IResult<&str, &str>{
    move |s: &str|{
        let are_same = opening == closing;
        let mut opening_count = 0_usize;
        let mut other_found = false;
        let mut closing_found = 0_usize;
        let mut retuval: IResult<&str, &str> = Ok(("", ""));
        let mut opening_bytes_len = 0_usize;
        let mut next_char_len: usize;
        let mut pos = 0_usize;
        let mut prev_pos = 0_usize;
        let mut before_closing_pos = 0_usize;

        let cc = s.chars();

        for next_char in cc {
            next_char_len = next_char.len_utf8();

            #[cfg(test)]
            println!("next_char: {}, len: {}", next_char, next_char_len);

            match next_char {
                c if (c == opening) && (!other_found) =>
                    {
                        opening_count += 1;
                        opening_bytes_len += next_char_len;
                    },
                c if (c == opening) && are_same && other_found && (opening_count > 0) =>
                    {
                        if closing_found == 0 {
                            before_closing_pos = prev_pos;
                        }

                        closing_found += 1;
                    },
                c if (c == closing) && (opening_count > 0) =>
                    {
                        if closing_found == 0 {
                            before_closing_pos = prev_pos;
                        }

                        closing_found += 1;
                        other_found = true;
                    },
                _ if opening_count == 0 =>
                    {
                        retuval =  std::result::Result::Err(
                            nom::Err::Error(
                                nom::error::Error::new(&s[0..=pos], ErrorKind::TakeUntil)
                            )
                        );
                        break;
                    },
                _ => 
                    {
                        other_found = true;
                        closing_found = 0;
                    },
            }

            if (opening_count > 0) && (opening_count == closing_found) {
                #[cfg(test)]
                println!("pos: {pos}");

                retuval = Ok((&s[pos + next_char_len..],&s[opening_bytes_len..=before_closing_pos]));
                break;
            }

            prev_pos = pos;
            pos += next_char_len;
        }

        if opening_count > closing_found {
            retuval =  std::result::Result::Err(
                nom::Err::Error(
                    nom::error::Error::new(s, ErrorKind::TakeUntil)
                )
            );
        }

        retuval
    }
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
                is_not(" \t\n\r/><"),
                many0(
                    preceded(
                        is_a(" \t\n\r"),
                        map(
                            many0(
                                alt((
                                    is_not(format!(" \t\n\r/><{}", LACONIC_SCRIPT_OPEN).as_str()),
                                    terminated(tag("/"), peek(is_not(">"))),
                                    delimited_by_matching_multiples(LACONIC_SCRIPT_OPEN, LACONIC_SCRIPT_CLOSE)
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
                    )
                ),
            ),
            |(first, list):(&str, Vec<String>)|
            {
                let mut result_vec = vec![];
                result_vec.push(first.to_string());

                for elem in list {
                    if !elem.is_empty() {
                        result_vec.push(elem);
                    }
                }

                result_vec
            }
        ),
        preceded(opt(whitespace), tag(closing_mark))
    ).parse(input)
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
    ).parse(input)
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
    ).parse(input)
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
    ).parse(input);

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
    match root.add_cargo_under_path(path, content) {
        Ok(added_path) => Ok(added_path),
        Err((err, _)) => Err(format!("{:?}", err)),
    }
}

fn borrow_content_from_root<'r>(root: &'r Node<NestedPageContent>, path: &Vec<usize>) -> Result<&'r NestedPageContent, String> {
    match root.borrow_cargo(path) {
        Ok(cargo) => Ok(cargo),
        Err(err) => Err(format!("{:?}", err)), 
    }
}

fn read_nested_content(input: &str) -> Result<Node<NestedPageContent>, String> {
    match read_flat_content(input) {
        Err(err) => Err(err),
        Ok(flat_contents) => {

            let mut root = Node::new(NestedPageContent::new(&["main".to_string()])?);
            let mut path = root.get_first_path();

            for flat_content in flat_contents {

                match flat_content {
                    FlatPageContent::Other(text) => {
                        add_content_to_root(&mut root, &path, NestedPageContent::new(&["other".to_string(), text])?)?;
                    },
                    FlatPageContent::SelfContainedMPTag(words) => {
                        if words[0].to_lowercase().as_str() == "actual" {
                            return Err("An <+actual ...>...</+actual> tag should have children, and shouldn't be self-contained like <+actual .../>.".to_string());
                        }

                        add_content_to_root(&mut root, &path, NestedPageContent::new(&words)?)?;
                    },
                    FlatPageContent::OpeningMPTag(words) => {
                        let new_cargo = match words[0].to_lowercase().as_str() {
                            "actual" => NestedPageContent::new(&words)?,
                            "comment" => NestedPageContent::new(&words)?,
                            _ => return Err("Invalid non self-contained masterpage tag found.".to_string()),
                        };

                        path = add_content_to_root(&mut root, &path, new_cargo)?;
                    },
                    FlatPageContent::ClosingMPTag(word) => {
                        // Check if the the parent NestedPageContent is the same variant.
                        let expected_variant = match word.to_lowercase().as_str() {
                            "actual" => NestedPageContent::Actual("dummy".to_string()),
                            "comment" => NestedPageContent::Comment,
                            _ => return Err("Invalid or unknown closing masterpage tag found.".to_string()),
                        };

                        let parent = borrow_content_from_root(&root, &path)?;

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
        let master_trees_result = content_tree.traverse_mut(
            Ok(Vec::<Node<NestedPageContent>>::new()),
            |trees_result: &mut Result<Vec<Node<NestedPageContent>>, String>, node, _path|{
                let mut do_go_on = true;

                if let NestedPageContent::Master(ref name) = node.cargo {
                    match read_text_from_handler(io_handler, OsStr::new(name)) {
                        Ok(master_text) => {
                            match read_nested_content(master_text.as_str()) {
                                Ok(master_tree) =>  {
                                    match trees_result {
                                        Ok(ref mut trees) => {
                                            trees.push(master_tree);

                                            let resolved = NestedPageContent::new(&["resolved".to_string(), name.clone()]);
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
                                    *trees_result = Err(err);
                                },
                            }
                        },
                        Err(err) => {
                            do_go_on = false;
                            *trees_result = Err(format!("Master file '{}' couldn't be read : {}.", name, err));
                        },
                    }
                }

                if do_go_on {TraverseAction::Continue} else {TraverseAction::Stop}
            }
        );

        match master_trees_result {
            Err(err) => return Err(err),
            Ok(master_trees) => {
                if master_trees.is_empty() {
                    break;
                }

                // Prepend all found master trees to the content tree's children.
                for master_tree in master_trees {
                    match content_tree.add_node_before_path(&vec![0], master_tree) {
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

            TraverseAction::Continue
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
                if !path.is_empty() {
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
                
                if *accum {TraverseAction::Stop} else {TraverseAction::Continue}
            }
        );

        if has_recursion {
            recursives.push(parent_name.clone());
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
                    TraverseAction::Stop
                },
                _ => TraverseAction::Continue,
            }
        }
    )
}

fn resolve_references_in_content_tree(content_tree: &mut Node<NestedPageContent>) -> Result<Vec<String>, String> {
    struct TraverseResult {
        unresolved_count: u32,
        resolved_count: u32,
    }

    let mut actuals = HashMap::<String, Node<NestedPageContent>>::new();
    let mut laconic_interpreter = Interpreter::new_stdio_filesys();
    let mut laconic_errors = Vec::<String>::new();

    #[cfg(test)]
    {
        println!("~~~~~~~~~~~~~~~~~~~~ Ref. resolution start : content_tree :\n{}",
            tests::utils::content_tree_to_string(content_tree));
    }

    let unresolved_tally = loop {

        // Add clones of already found actuals to hashmap.
        actuals = content_tree.traverse(
            actuals,
            |acts, node, _path| {
                if let NestedPageContent::Actual(ref name) = node.cargo {
                    acts.entry(name.clone()).or_insert((*node).clone());
                }

                TraverseAction::Continue
            }
        );

        // Resolve placeholders and calc operands that can already be resolved.
        let traverse_results = content_tree.traverse_mut(
            TraverseResult{unresolved_count: 0, resolved_count:0},
            |trav_results, node, _path| {
                match node.cargo {
                    NestedPageContent::PlaceHolder(ref name) => {
                        match actuals.get(name) {
                            None => trav_results.unresolved_count += 1,
                            Some(actual_node) => {
                                trav_results.resolved_count += 1;
                                node.cargo = NestedPageContent::Resolved(name.clone());
                                node.children.clear();

                                // Add cloned children from the actual node to the current node.
                                for child in &actual_node.children {
                                    node.children.push(child.clone());
                                }
                            },
                        }
                    },
                    NestedPageContent::Laconic(ref mut laconic_script) => {
                        // Try and execute the Laconic script.
                        match laconic_script.get_value(&mut laconic_interpreter) {
                            LaconicValue::Unresolved => (),
                            LaconicValue::Resolved(ref val) => {
                                node.cargo = match laconic_script.name.as_str() {
                                    "-" => NestedPageContent::Resolved(laconic_script.name.clone()),
                                    nm => NestedPageContent::Actual(nm.to_string()),
                                };

                                node.children.clear();
                                node.children.push(Node::new(NestedPageContent::Other(val.to_string())));
                            },
                            LaconicValue::Invalid(script_err) => {
                                let script_text = laconic_script.script.clone();
                                node.cargo = NestedPageContent::Resolved(laconic_script.name.clone());

                                node.children.clear();
                                node.children.push(Node::new(NestedPageContent::Other(script_err.to_string())));
                                laconic_errors.push(format!("{} in script\n'{{{}}}'", script_err, script_text));
                            },
                        }
                    },
                    NestedPageContent::Calc(ref mut calculation) => {
                        // Check if any the calculation's operands can be resolved.
                        // If so, resolve them.
                        // If all can be resolved, replace the NestedPageContent::Calc cargo with
                        // an NestedPageContent::Actual cargo, except when the name is "-",
                        // in which case a NestedPageContent::Other cargo is produced.

                        for operand in &mut calculation.operands {
                            if let Operand::PlaceHolder(ref name) = operand {

                                #[cfg(test)]
                                {
                                    println!("Looking for an actual for {}", name);
                                }

                                match actuals.get_mut(name) {
                                    None => trav_results.unresolved_count += 1,
                                    Some(ref mut actual_node) => {
                                        // Read the content of the first Other child.
                                        let num_text = actual_node.traverse(
                                            String::new(),
                                            |accum, nd, _path| {
                                                if let NestedPageContent::Other(ref txt) = nd.cargo {
                                                    *accum = txt.clone();
                                                }

                                                if accum.is_empty() {TraverseAction::Continue} else {TraverseAction::Stop}
                                            }
                                        );

                                        #[cfg(test)]
                                        {
                                            println!("num_text = {}", &num_text);
                                        }

                                        if !num_text.is_empty() {
                                            let opd = Operand::new(num_text);

                                            match opd {
                                                Operand::Value(_) => {
                                                    *operand = opd;
                                                    trav_results.resolved_count += 1;
                                                },
                                                _ => trav_results.unresolved_count += 1,

                                            }
                                        } else {
                                            trav_results.unresolved_count += 1;
                                        }
                                    },
                                }
                            }
                        }

                        // Try and resolve the calculation.
                        match calculation.get_value() {
                            CalculationValue::Unresolved => (),
                            CalculationValue::Resolved(ref val) => {
                                node.cargo = match calculation.name.as_str() {
                                    "-" => NestedPageContent::Resolved(calculation.name.clone()),
                                    nm => NestedPageContent::Actual(nm.to_string()),
                                };

                                node.children.clear();
                                node.children.push(Node::new(NestedPageContent::Other(val.to_string())));
                            },
                            CalculationValue::Invalid(calc_err) => {
                                node.cargo = NestedPageContent::Resolved(calculation.name.clone());

                                node.children.clear();
                                node.children.push(Node::new(NestedPageContent::Other(calc_err.to_string())));
                            },
                        }
                    },
                    _ => (),
                }

                TraverseAction::Continue
            }
        );

        #[cfg(test)]
        {
            println!("~~~~~~~~~~~~~~~~~~~~ Unresoldved: {} ~~~~ Resolved: {}",
                traverse_results.unresolved_count,
                traverse_results.resolved_count
            );

            println!("~~~~~~~~~~~~~~~~~~~~ Ref. resolution loop  : content_tree :\n{}",
                tests::utils::content_tree_to_string(content_tree));
        }

        if (traverse_results.unresolved_count == 0) || (traverse_results.resolved_count == 0) {
            break traverse_results.unresolved_count;
        }
    };

    if unresolved_tally == 0 {
        Ok(laconic_errors)
    } else {
        Err("Unresolvable placeholder tags or calculation operands found.".to_string())
    }
}

fn content_tree_to_output_string(content_tree: &mut Node<NestedPageContent>) -> String {
    content_tree.traverse(
        String::new(),
        |accum, node, _path| {
            // Skip actual and comment nodes entirely,
            // because they can contain Other and Resolved nodes
            // that shouldn't be enclosed in the output.

            match node.cargo {
                NestedPageContent::Actual(_) => TraverseAction::SkipChildren,
                NestedPageContent::Comment => TraverseAction::SkipChildren,
                NestedPageContent::Other(ref content) => {
                    accum.push_str(content);
                    TraverseAction::Continue
                },
                NestedPageContent::Timestamp => {
                    let duration = loop {
                        if let Ok(dur) = SystemTime::now().duration_since(SystemTime::UNIX_EPOCH) {
                            break dur;
                        }
                    };

                    accum.push_str("ts=");
                    accum.push_str(&duration.as_millis().to_string());
                    TraverseAction::Continue
                },
                _ => TraverseAction::Continue,
            }
        }
    ).trim().to_string()
}

#[cfg(test)]
mod tests {
    use super::*;
    use nom::{
        bytes::complete::{is_a, tag}
    };
    use string_io_and_mock::MockTextHandler;
    use tree_by_path::Node;

    pub mod utils {
        use crate::NestedPageContent;
        use tree_by_path::{Node, TraverseAction};

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

                    TraverseAction::Continue
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
    fn mpt_self_contained_braces() {
        let result = master_page_tag_self_contained(r#"<+abc / {/>4 3 1 <+} def ghi/>blabla"#);
        assert!(result.is_ok());
        assert_eq!(Ok(("blabla", FlatPageContent::SelfContainedMPTag(vec!["abc".to_string(), "/".to_string(), "/>4 3 1 <+".to_string(), "def".to_string(), "ghi".to_string()]))), result);
    }

    #[test]
    fn mpt_self_contained_braces_with_newline() {
        let result = master_page_tag_self_contained("<+abc / {/>4 3
1 <+} def ghi/>blabla");

        // Debug
        // println!("master_page_tag_self_contained result: {}", result.unwrap_err());

        assert!(result.is_ok());
        assert_eq!(Ok(("blabla", FlatPageContent::SelfContainedMPTag(vec!["abc".to_string(), "/".to_string(), "/>4 3\n1 <+".to_string(), "def".to_string(), "ghi".to_string()]))), result);
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
        let result = read_flat_content(test_content);
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

        let result = read_flat_content(test_content);
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

        let result = read_flat_content(test_content);
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
    fn read_nested_content_mixed_with_laconic() {
        let test_content = r#"
<+output out.htm/>
<+master boilerplate.mpm/>
<+actual title>Introduction</+actual>
<+laconic - {{
$#width 12.4
}}/>
<+actual body>
Welcome to my site about <+placeholder title/>!
</+actual>
"#.replace("\n", "");

        let result = read_nested_content(test_content.as_str());
        assert!(result.is_ok());

        let mut root = result.unwrap();
        let repr = utils::content_tree_to_string(&mut root);

        assert_eq!(
            "Main[Output(out.htm) Master(boilerplate.mpm) Actual(title)[Other(Introducti)] Laconic(- $#width 12.4) Actual(body)[Other(Welcome to) PlaceHolder(title) Other(!)]]".to_string(),
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
        
        // Debug
        // println!("{}", utils::content_tree_to_string(&mut content_tree));

        assert_eq!(
            "Main[Actual(title)[Other(Test page)] Other(<!doctype ) Resolved(title)[Other(Test page)] Other(</title></) Resolved(page_content)[Other(This is a )] Other(</body></h) Actual(page_content)[Other(This is a )]]".to_string(),
            utils::content_tree_to_string(&mut content_tree)
        );
    }

    #[test]
    fn resolve_references_calc() {
        let content_source_str = "
<+actual start>70</+actual>
<+actual sub>10</+actual>
<+calc AAA - start sub/>
<+calc - / AAA BBB/>
<+calc BBB min start 25/>
".replace("\n", "");

        let content_source = content_source_str.as_str();
        let mut content_tree = read_nested_content(content_source).unwrap();
        let result = resolve_references_in_content_tree(&mut content_tree);
        assert!(result.is_ok());

        // Debug
        // println!("{}", utils::content_tree_to_string(&mut content_tree));

        assert_eq!(
            "Main[Actual(start)[Other(70)] Actual(sub)[Other(10)] Actual(AAA)[Other(60)] Resolved(-)[Other(2.4)] Actual(BBB)[Other(25)]]".to_string(),
            utils::content_tree_to_string(&mut content_tree)
        );
    }

    #[test]
    fn calc_resolve_unresolved_operand() {
        let words = vec!["test", "+", "-19.2", "width"];
        let mut c = Calculation::new(words.iter().map(|w| w.to_string()).collect()).unwrap();
        
        let mut outcome = c.get_value();
        assert_eq!(CalculationValue::Unresolved, outcome);

        c.operands[1] = Operand::Value(10_f64);
        outcome = c.get_value();
        assert_eq!(CalculationValue::Resolved(-9.2_f64), outcome);
    }

    #[test]
    fn calc_resolve_add_2() {
        let words = vec!["test", "+", "-19.2", "40"];
        let mut c = Calculation::new(words.iter().map(|w| w.to_string()).collect()).unwrap();

        for _ in 0..2 {
            let outcome = c.get_value();
            assert_eq!(CalculationValue::Resolved(20.8_f64), outcome);
        }
    }

    #[test]
    fn calc_resolve_multiply_4() {
        let words = vec!["test", "*", "2", "3", "4", "5"];
        let mut c = Calculation::new(words.iter().map(|w| w.to_string()).collect()).unwrap();

        let outcome = c.get_value();
        assert_eq!(CalculationValue::Resolved(120_f64), outcome);
    }

    #[test]
    fn calc_resolve_subtract_none() {
        let words = vec!["test", "-"];
        let mut c = Calculation::new(words.iter().map(|w| w.to_string()).collect()).unwrap();

        let outcome = c.get_value();
        assert_eq!(CalculationValue::Resolved(0_f64), outcome);
    }

    #[test]
    fn calc_resolve_subtract_1() {
        let words = vec!["test", "-", "18"];
        let mut c = Calculation::new(words.iter().map(|w| w.to_string()).collect()).unwrap();

        let outcome = c.get_value();
        assert_eq!(CalculationValue::Resolved(18_f64), outcome);
    }

    #[test]
    fn calc_resolve_subtract_2() {
        let words = vec!["test", "-", "100.7", "20.3"];
        let mut c = Calculation::new(words.iter().map(|w| w.to_string()).collect()).unwrap();

        let outcome = c.get_value();
        assert_eq!(CalculationValue::Resolved(80.4_f64), outcome);
    }

    #[test]
    fn calc_resolve_divide_3() {
        let words = vec!["test", "/", "30", "5", "2"];
        let mut c = Calculation::new(words.iter().map(|w| w.to_string()).collect()).unwrap();

        let outcome = c.get_value();
        assert_eq!(CalculationValue::Resolved(3_f64), outcome);
    }

    #[test]
    fn calc_resolve_divide_by_zero() {
        let words = vec!["test", "/", "30", "0", "2"];
        let mut c = Calculation::new(words.iter().map(|w| w.to_string()).collect()).unwrap();

        let outcome = c.get_value();
        assert_eq!(CalculationValue::Invalid(CalcError::DivisionByZero), outcome);
    }

    #[test]
    fn calc_resolve_min_4() {
        let words = vec!["test", "min", "30", "5.5", "-2", "500"];
        let mut c = Calculation::new(words.iter().map(|w| w.to_string()).collect()).unwrap();

        let outcome = c.get_value();
        assert_eq!(CalculationValue::Resolved(-2_f64), outcome);
    }

    #[test]
    fn calc_resolve_min_1() {
        let words = vec!["test", "min", "30"];
        let mut c = Calculation::new(words.iter().map(|w| w.to_string()).collect()).unwrap();

        let outcome = c.get_value();
        assert_eq!(CalculationValue::Resolved(30_f64), outcome);
    }

    #[test]
    fn calc_resolve_min_0() {
        let words = vec!["test", "min"];
        let mut c = Calculation::new(words.iter().map(|w| w.to_string()).collect()).unwrap();

        let outcome = c.get_value();
        assert_eq!(CalculationValue::Resolved(f64::MAX), outcome);
    }

    #[test]
    fn calc_resolve_max_0() {
        let words = vec!["test", "max"];
        let mut c = Calculation::new(words.iter().map(|w| w.to_string()).collect()).unwrap();

        let outcome = c.get_value();
        assert_eq!(CalculationValue::Resolved(f64::MIN), outcome);
    }

    #[test]
    fn calc_resolve_max_1() {
        let words = vec!["test", "max", "-13.333"];
        let mut c = Calculation::new(words.iter().map(|w| w.to_string()).collect()).unwrap();

        let outcome = c.get_value();
        assert_eq!(CalculationValue::Resolved(-13.333_f64), outcome);
    }

    #[test]
    fn calc_resolve_max_2() {
        let words = vec!["test", "max", "100", "100.01"];
        let mut c = Calculation::new(words.iter().map(|w| w.to_string()).collect()).unwrap();

        let outcome = c.get_value();
        assert_eq!(CalculationValue::Resolved(100.01_f64), outcome);
    }

    #[test]
    fn calc_resolve_abs_pos() {
        let words = vec!["test", "abs", "100"];
        let mut c = Calculation::new(words.iter().map(|w| w.to_string()).collect()).unwrap();

        let outcome = c.get_value();
        assert_eq!(CalculationValue::Resolved(100_f64), outcome);
    }

    #[test]
    fn calc_resolve_abs_neg() {
        let words = vec!["test", "abs", "-100"];
        let mut c = Calculation::new(words.iter().map(|w| w.to_string()).collect()).unwrap();

        let outcome = c.get_value();
        assert_eq!(CalculationValue::Resolved(100_f64), outcome);
    }

    #[test]
    fn calc_resolve_remainder_2() {
        let words = vec!["test", "%", "100", "7"];
        let mut c = Calculation::new(words.iter().map(|w| w.to_string()).collect()).unwrap();

        let outcome = c.get_value();
        assert_eq!(CalculationValue::Resolved(2_f64), outcome);
    }

    #[test]
    fn calc_resolve_remainder_1() {
        let words = vec!["test", "%", "73",];
        let mut c = Calculation::new(words.iter().map(|w| w.to_string()).collect()).unwrap();

        let outcome = c.get_value();
        assert_eq!(CalculationValue::Resolved(73_f64), outcome);
    }

    #[test]
    fn calc_resolve_exp_positive_int() {
        let words = vec!["test", "^", "5", "3"];
        let mut c = Calculation::new(words.iter().map(|w| w.to_string()).collect()).unwrap();

        let outcome = c.get_value();
        assert_eq!(CalculationValue::Resolved(125_f64), outcome);
    }

    #[test]
    fn calc_resolve_exp_negative_int() {
        let words = vec!["test", "**", "4", "-1"];
        let mut c = Calculation::new(words.iter().map(|w| w.to_string()).collect()).unwrap();

        let outcome = c.get_value();
        assert_eq!(CalculationValue::Resolved(0.25_f64), outcome);
    }

    #[test]
    fn calc_resolve_exp_positive_fractal() {
        let words = vec!["test", "pow", "81", ".5"];
        let mut c = Calculation::new(words.iter().map(|w| w.to_string()).collect()).unwrap();

        let outcome = c.get_value();
        assert_eq!(CalculationValue::Resolved(9_f64), outcome);
    }

    #[test]
    fn calc_resolve_exp_negative_fractal() {
        let words = vec!["test", "exp", "100", "-.5"];
        let mut c = Calculation::new(words.iter().map(|w| w.to_string()).collect()).unwrap();

        let outcome = c.get_value();
        assert_eq!(CalculationValue::Resolved(0.1_f64), outcome);
    }

    #[test]
    fn calc_resolve_exp_series() {
        let words = vec!["test", "^", "2", "4", ".5", ".5"];
        let mut c = Calculation::new(words.iter().map(|w| w.to_string()).collect()).unwrap();

        let outcome = c.get_value();
        assert_eq!(CalculationValue::Resolved(2_f64), outcome);
    }

    #[test]
    fn calc_resolve_sign_positive() {
        let words = vec!["test", "sign", "5"];
        let mut c = Calculation::new(words.iter().map(|w| w.to_string()).collect()).unwrap();

        let outcome = c.get_value();
        assert_eq!(CalculationValue::Resolved(1_f64), outcome);
    }

    #[test]
    fn calc_resolve_sign_negative() {
        let words = vec!["test", "sign", "-56.412"];
        let mut c = Calculation::new(words.iter().map(|w| w.to_string()).collect()).unwrap();

        let outcome = c.get_value();
        assert_eq!(CalculationValue::Resolved(-1_f64), outcome);
    }

    #[test]
    fn calc_resolve_sign_series() {
        let words = vec!["test", "sign", "-56.412", "2", "-4.4"];
        let mut c = Calculation::new(words.iter().map(|w| w.to_string()).collect()).unwrap();

        let outcome = c.get_value();
        assert_eq!(CalculationValue::Resolved(1_f64), outcome);
    }

    #[test]
    fn calc_resolve_round_positive_small() {
        let words = vec!["test", "round", "56.41"];
        let mut c = Calculation::new(words.iter().map(|w| w.to_string()).collect()).unwrap();

        let outcome = c.get_value();
        assert_eq!(CalculationValue::Resolved(56_f64), outcome);
    }

    #[test]
    fn calc_resolve_round_positive_large() {
        let words = vec!["test", "round", "56.5"];
        let mut c = Calculation::new(words.iter().map(|w| w.to_string()).collect()).unwrap();

        let outcome = c.get_value();
        assert_eq!(CalculationValue::Resolved(57_f64), outcome);
    }

    #[test]
    fn calc_resolve_round_negative_small() {
        let words = vec!["test", "round", "-6.41"];
        let mut c = Calculation::new(words.iter().map(|w| w.to_string()).collect()).unwrap();

        let outcome = c.get_value();
        assert_eq!(CalculationValue::Resolved(-6_f64), outcome);
    }

    #[test]
    fn calc_resolve_round_negative_large() {
        let words = vec!["test", "round", "-6.5"];
        let mut c = Calculation::new(words.iter().map(|w| w.to_string()).collect()).unwrap();

        let outcome = c.get_value();
        assert_eq!(CalculationValue::Resolved(-7_f64), outcome);
    }

    #[test]
    fn calc_resolve_trunc_positive_small() {
        let words = vec!["test", "trunc", "56.41"];
        let mut c = Calculation::new(words.iter().map(|w| w.to_string()).collect()).unwrap();

        let outcome = c.get_value();
        assert_eq!(CalculationValue::Resolved(56_f64), outcome);
    }

    #[test]
    fn calc_resolve_trunc_positive_large() {
        let words = vec!["test", "trunc", "56.5"];
        let mut c = Calculation::new(words.iter().map(|w| w.to_string()).collect()).unwrap();

        let outcome = c.get_value();
        assert_eq!(CalculationValue::Resolved(56_f64), outcome);
    }

    #[test]
    fn calc_resolve_trunc_negative_small() {
        let words = vec!["test", "trunc", "-6.41"];
        let mut c = Calculation::new(words.iter().map(|w| w.to_string()).collect()).unwrap();

        let outcome = c.get_value();
        assert_eq!(CalculationValue::Resolved(-6_f64), outcome);
    }

    #[test]
    fn calc_resolve_trunc_negative_large() {
        let words = vec!["test", "trunc", "-6.5"];
        let mut c = Calculation::new(words.iter().map(|w| w.to_string()).collect()).unwrap();

        let outcome = c.get_value();
        assert_eq!(CalculationValue::Resolved(-6_f64), outcome);
    }

    #[test]
    fn calc_resolve_floor_positive_small() {
        let words = vec!["test", "floor", "56.41"];
        let mut c = Calculation::new(words.iter().map(|w| w.to_string()).collect()).unwrap();

        let outcome = c.get_value();
        assert_eq!(CalculationValue::Resolved(56_f64), outcome);
    }

    #[test]
    fn calc_resolve_floor_positive_large() {
        let words = vec!["test", "floor", "56.5"];
        let mut c = Calculation::new(words.iter().map(|w| w.to_string()).collect()).unwrap();

        let outcome = c.get_value();
        assert_eq!(CalculationValue::Resolved(56_f64), outcome);
    }

    #[test]
    fn calc_resolve_floor_negative_small() {
        let words = vec!["test", "floor", "-6.41"];
        let mut c = Calculation::new(words.iter().map(|w| w.to_string()).collect()).unwrap();

        let outcome = c.get_value();
        assert_eq!(CalculationValue::Resolved(-7_f64), outcome);
    }

    #[test]
    fn calc_resolve_floor_negative_large() {
        let words = vec!["test", "floor", "-6.5"];
        let mut c = Calculation::new(words.iter().map(|w| w.to_string()).collect()).unwrap();

        let outcome = c.get_value();
        assert_eq!(CalculationValue::Resolved(-7_f64), outcome);
    }

    #[test]
    fn calc_resolve_ceiling_positive_small() {
        let words = vec!["test", "ceiling", "56.41"];
        let mut c = Calculation::new(words.iter().map(|w| w.to_string()).collect()).unwrap();

        let outcome = c.get_value();
        assert_eq!(CalculationValue::Resolved(57_f64), outcome);
    }

    #[test]
    fn calc_resolve_ceiling_positive_large() {
        let words = vec!["test", "ceiling", "56.5"];
        let mut c = Calculation::new(words.iter().map(|w| w.to_string()).collect()).unwrap();

        let outcome = c.get_value();
        assert_eq!(CalculationValue::Resolved(57_f64), outcome);
    }

    #[test]
    fn calc_resolve_ceiling_negative_small() {
        let words = vec!["test", "ceiling", "-6.41"];
        let mut c = Calculation::new(words.iter().map(|w| w.to_string()).collect()).unwrap();

        let outcome = c.get_value();
        assert_eq!(CalculationValue::Resolved(-6_f64), outcome);
    }

    #[test]
    fn calc_resolve_ceiling_negative_large() {
        let words = vec!["test", "ceiling", "-6.5"];
        let mut c = Calculation::new(words.iter().map(|w| w.to_string()).collect()).unwrap();

        let outcome = c.get_value();
        assert_eq!(CalculationValue::Resolved(-6_f64), outcome);
    }

    #[test]
    fn calc_resolve_upper_case_operator() {
        let words = vec!["test", "SIGN", "-56.412"];
        let mut c = Calculation::new(words.iter().map(|w| w.to_string()).collect()).unwrap();

        let outcome = c.get_value();
        assert_eq!(CalculationValue::Resolved(-1_f64), outcome);
    }

    #[test]
    fn compose_has_unresolved_placeholders() {
        let content_text = "
<+output art.htm/>
<+actual webData>
e-mail: gencalogero@famous_server.com<br />
</+actual>
<body>
<+placeholder telephone/>
</body>
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
            "Unresolvable placeholder tags or calculation operands found.",
            result.unwrap_err()
        );

        let read_output_result = text_handler.read_text(&OsStr::new("art.htm"));
        assert!(read_output_result.is_err());
        assert_eq!(std::io::ErrorKind::NotFound, read_output_result.unwrap_err().kind());
    }

    #[test]
    fn compose_has_unresolved_calculation_operands() {
        let content_text = "
<+output art.htm/>
<+actual webData>
e-mail: gencalogero@famous_server.com<br />
</+actual>
<body>
<+placeholder webData/>
<+calc - + 20 subTotal/>
</body>
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
            "Unresolvable placeholder tags or calculation operands found.",
            result.unwrap_err()
        );

        let read_output_result = text_handler.read_text(&OsStr::new("art.htm"));
        assert!(read_output_result.is_err());
        assert_eq!(std::io::ErrorKind::NotFound, read_output_result.unwrap_err().kind());
    }

    #[test]
    fn tree_to_output() {
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
<+placeholder title/><br />
This is a test page.<br />
<+comment>This is just any calculation.</+comment>
Calculation outcome is: <+calc - / AAA BBB/>
</+actual>
<+actual start>70</+actual>
<+actual sub>10</+actual>
<+calc AAA - start sub/>
<+calc BBB min start 25/>
".replace("\n","");

        let mut content_tree = read_nested_content(content_source_str.as_str()).unwrap();
        resolve_references_in_content_tree(&mut content_tree).unwrap();
        let output = content_tree_to_output_string(&mut content_tree);

        // Debug
        // println!("Output string:\n{}", &output);

        let expected = "
<!doctype html>
<html>
<head>
<title>Test page</title>
</head>
<body>Test page<br />
This is a test page.<br />
Calculation outcome is: 2.4</body>
</html>
".replace("\n","");

        assert_eq!(expected, output);
    }

    #[test]
    fn tree_to_output_timestamp() {
        let content_source_str =
            "OK then, the timestamp is : <+timestamp/>. Happy now ?";

        let mut content_tree = read_nested_content(content_source_str).unwrap();
        let output = content_tree_to_output_string(&mut content_tree);

        /* Compiles also, but the nested function below looks cleaner.
            let begin = "OK then, the timestamp is : ts=";
            let end = ". Happy now ?";

            let mut parser = tuple((
                tag::<&str, &str, nom::error::Error<&str>>(begin),
                is_a("0123456789"),   
                tag::<&str, &str, nom::error::Error<&str>>(end),
            ));
        */

        fn parser(input: &str) -> nom:: IResult<&str, (&str, &str, &str)> {
            // Trait Parser is implemented directly on tuples. So : (_,_).parse(input)
            (
                tag("OK then, the timestamp is : ts="),
                is_a("0123456789"),   
                tag(". Happy now ?"),
            ).parse(input)
        }

        let parse_result = parser(output.as_str());

        assert!(parse_result.is_ok());
    }

    #[test]
    fn compose_with_master() {
        let master_source = "
<!doctype html>
<html>
<head>
<+comment>Keep it simple for now.</+comment>
<title><+placeholder title/></title>
</head>
<body>
<+PLACEHOLDER page_content/>
</body>
</html>
".replace("\n", "");

        let page_content_source ="
<+master general.mpm/>
<+OUTPUT testPage.htm/>

<+actual title>Test page</+actual>

<+Actual page_content>
<+placeholder title/><br />
This is a test page.<br />
Calculation outcome is: <+calc - / AAA BBB/>
</+actual>

<+actual start>70</+actual>
<+actual sub>10</+actual>
<+calc AAA - start sub/>
<+calc BBB min start 25/>
".replace("\n", "");

        let master_name = OsStr::new("general.mpm");
        let client_name = OsStr::new("testPage.mpc");

        let mut text_handler = MockTextHandler::new();
        text_handler.write_text(&master_name, master_source).unwrap();
        text_handler.write_text(&client_name, page_content_source).unwrap();

        let result = compose(&client_name, &mut text_handler);

        // Debug
        // println!("Error = {}", result.unwrap_err());

        assert!(result.is_ok());

        let output = text_handler.read_text(&OsStr::new("testPage.htm")).unwrap();

        // Debug
        // println!("Composed file:\n{}", &output);

        let expected = "
<!doctype html>
<html>
<head>
<title>Test page</title>
</head>
<body>Test page<br />
This is a test page.<br />
Calculation outcome is: 2.4</body>
</html>
".replace("\n", "").to_string();

        assert_eq!(expected, output);
    }

    #[test]
    fn compose_with_laconic() {
        let master_source = r#"
<!doctype html>
<html>
<head>
<+comment>Keep it simple for now.</+comment>
<title><+placeholder title/></title>
</head>
<body>
<+laconic FIB4 {
$#fib4 o#fib 4 #
}/>
<+PLACEHOLDER page_content/>
</body>
</html>
"#.replace("\n", "");

        let page_content_source = r#"
<+master general.mpm/>
<+OUTPUT testPage.htm/>

<+actual title>Test page</+actual>

<+actual page_content>
<+placeholder title/><br />
This is a test page.<br />
Fibonacci(4) is: <+laconic - {q,v#fib4}/>
</+actual>
"#.replace("\n", "");

        let master_name = OsStr::new("general.mpm");
        let client_name = OsStr::new("testPage.mpc");

        let mut text_handler = MockTextHandler::new();
        text_handler.write_text(&master_name, master_source).unwrap();
        text_handler.write_text(&client_name, page_content_source).unwrap();

        let result = compose(&client_name, &mut text_handler);

        // Debug
        // println!("Error = {}", result.unwrap_err());

        assert!(result.is_ok());

        let output = text_handler.read_text(&OsStr::new("testPage.htm")).unwrap();

        // Debug
        // println!("Composed file:\n{}", &output);

        let expected = "
<!doctype html>
<html>
<head>
<title>Test page</title>
</head>
<body>Test page<br />
This is a test page.<br />
Fibonacci(4) is: 3
</body>
</html>
".replace("\n", "").to_string();

        assert_eq!(expected, output);
    }

    #[test]
    fn compose_missing_master() {
        let page_content_source ="
<+master general.mpm/>
<+output testPage.htm/>

<+actual title>Test page</+actual>

<+actual page_content>
<+placeholder title/><br />
This is a test page.<br />
Calculation outcome is: <+calc - / AAA BBB/>
</+actual>

<+actual start>70</+actual>
<+actual sub>10</+actual>
<+calc AAA - start sub/>
<+calc BBB min start 25/>
".trim();

        let client_name = OsStr::new("testPage.mpc");

        let mut text_handler = MockTextHandler::new();
        text_handler.write_text(&client_name, page_content_source.to_string()).unwrap();

        let result = compose(&client_name, &mut text_handler);
        assert!(result.is_err());
        assert!(result.unwrap_err().starts_with("Master file 'general.mpm' couldn't be read"));
        assert!(text_handler.read_text(&OsStr::new("testPage.htm")).is_err());
    }

    #[test]
    fn compose_missing_output() {
        let master_source = "
<!doctype html>
<html>
<head>
<title><+placeholder title/></title>
</head>
<body>
<+placeholder page_content/>
</body>
</html>
".trim();

        let page_content_source ="
<+master general.mpm/>

<+actual title>Test page</+actual>

<+actual page_content>
<+placeholder title/><br />
This is a test page.<br />
Calculation outcome is: <+calc - / AAA BBB/>
</+actual>

<+actual start>70</+actual>
<+actual sub>10</+actual>
<+calc AAA - start sub/>
<+calc BBB min start 25/>
".trim();

        let master_name = OsStr::new("general.mpm");
        let client_name = OsStr::new("testPage.mpc");

        let mut text_handler = MockTextHandler::new();
        text_handler.write_text(&master_name, master_source.to_string()).unwrap();
        text_handler.write_text(&client_name, page_content_source.to_string()).unwrap();

        let result = compose(&client_name, &mut text_handler);
        assert!(result.is_err());
        assert_eq!("No output name found.".to_string(), result.unwrap_err());
    }

    #[test]
    fn compose_flat_text_only() {
        let page_source = "
<+output testPage.htm/>
<!doctype html>
<html>
<head>
<title>Test page</title>
</head>
<body>
Oh, never mind.
</body>
</html>
".to_string();

        let mut text_handler = MockTextHandler::new();
        let client_name = OsStr::new("testPage.mpc");
        text_handler.write_text(&client_name, page_source).unwrap();

        let result = compose(&client_name, &mut text_handler);
        assert!(result.is_ok());

        let output = text_handler.read_text(&OsStr::new("testPage.htm")).unwrap();
    
        // Debug
        // println!("Composed file:\n{}", &output);

        let expected = "
<!doctype html>
<html>
<head>
<title>Test page</title>
</head>
<body>
Oh, never mind.
</body>
</html>
".trim().to_string();

        assert_eq!(expected, output);
    }

    mod delimited_by_same_nr {
        use crate::delimited_by_matching_multiples;

        use nom::{
            bytes::complete::{is_a, tag},
            error::ErrorKind,
            IResult,
            Parser,
            sequence::{delimited, preceded}
        };

        #[test]
        fn simple_is_a() {
            fn parse_it(s: &str) -> IResult<&str, &str> {
                delimited(
                    is_a("{"),
                    is_a("abc"),
                    is_a("}")
                ).parse(s)
            }

            assert_eq!(parse_it("{{{aaa}}}"), Ok(("", "aaa")));
        }

        #[test]
        fn three_braces_simple() {
            assert_eq!(
                delimited_by_matching_multiples('{', '}')("{{{aaa}}}xxx"),
                Ok(("xxx", "aaa"))
            );
        }

        #[test]
        fn three_braces_surplus(){
            assert_eq!(
                delimited_by_matching_multiples('{', '}')("{{{aaa}}}}xxx"),
                Ok(("}xxx", "aaa"))
            );
        }

        #[test]
        fn two_braces_single_content_braces_pair() {
            assert_eq!(
                delimited_by_matching_multiples('{', '}')("{{aaa {---} bbb}}xxx"),
                Ok(("xxx", "aaa {---} bbb"))
            );
        }

        #[test]
        fn two_braces_single_content_opening_brace() {
            assert_eq!(
                delimited_by_matching_multiples('{', '}')("{{aaa { bbb}}xxx"),
                Ok(("xxx", "aaa { bbb"))
            );
        }

        #[test]
        fn two_braces_single_content_closing_brace() {
            assert_eq!(
                delimited_by_matching_multiples('{', '}')("{{aaa } bbb}}xxx"),
                Ok(("xxx", "aaa } bbb"))
            );
        }

        #[test]
        fn single_braces_simple() {
            assert_eq!(
                delimited_by_matching_multiples('{', '}')("{aaa}{xxx"),
                Ok(("{xxx", "aaa"))
            );
        }

        #[test]
        fn combined() {
            assert_eq!(
                preceded(
                    tag("???"),
                    delimited_by_matching_multiples('{', '}')
                ).parse("???{aaa}{xxx"),

                Ok(("{xxx", "aaa"))
            );
        }

        #[test]
        fn two_opening_one_closing() {
            assert_eq!(
                delimited_by_matching_multiples('{', '}')("{{aaa}xxx"),
                std::result::Result::Err(nom::Err::Error(nom::error::Error::new("{{aaa}xxx", ErrorKind::TakeUntil)))
            );
        }

        #[test]
        fn no_braces() {
            assert_eq!(
                delimited_by_matching_multiples('{', '}')("cccxxx"),
                std::result::Result::Err(nom::Err::Error(nom::error::Error::new("c", ErrorKind::TakeUntil)))
            );
        }

        #[test]
        fn closing_braces_at_start() {
            assert_eq!(
                delimited_by_matching_multiples('{', '}')("}cccxxx"),
                std::result::Result::Err(nom::Err::Error(nom::error::Error::new("}", ErrorKind::TakeUntil)))
            );
        }

        #[test]
        fn same_single() {
            assert_eq!(
                delimited_by_matching_multiples('*', '*')("*aaa*{xxx"),
                Ok(("{xxx", "aaa"))
            );
        }

        #[test]
        fn same_double() {
            assert_eq!(
                delimited_by_matching_multiples('*', '*')("**aaa*bbb*ccc**xxx"),
                Ok(("xxx", "aaa*bbb*ccc"))
            );
        }

        #[test]
        fn same_surplus() {
            assert_eq!(
                delimited_by_matching_multiples('*', '*')("*aaa**xxx"),
                Ok(("*xxx", "aaa"))
            );
        }

        #[test]
        fn high_chars_in_content() {
            assert_eq!(
                delimited_by_matching_multiples('<', '>')("<--€-經-->xxx"),
                Ok(("xxx", "--€-經--"))
            );
        }

        #[test]
        fn high_chars_as_delimiters() {
            assert_eq!(
                delimited_by_matching_multiples('☳', '☶')("☳---☶xxx"),
                Ok(("xxx", "---"))
            );
        }

        #[test]
        fn mixed_len_chars_as_delimiters_1() {
            assert_eq!(
                delimited_by_matching_multiples('€', '|')("€---|₭xxx"),
                Ok(("₭xxx", "---"))
            );
        }

        #[test]
        fn mixed_len_chars_as_delimiters_2() {
            assert_eq!(
                delimited_by_matching_multiples('|', '€')("|---€₭xxx"),
                Ok(("₭xxx", "---"))
            )
        }

        #[test]
        fn high_chars_as_both() {
            assert_eq!(
                delimited_by_matching_multiples('☳', '☶')("☳-經-☶€xxx"),
                Ok(("€xxx", "-經-"))
            );
        }

        #[test]
        fn not_same_empty() {
            assert_eq!(
                delimited_by_matching_multiples('<', '>')("<>xxx"),
                Ok(("xxx", ""))
            );
        }

        #[test]
        fn same_empty() {
            assert_eq!(
                delimited_by_matching_multiples('*', '*')("**xxx"),
                std::result::Result::Err(nom::Err::Error(nom::error::Error::new("**xxx", ErrorKind::TakeUntil)))
            );
        }

        #[test]
        fn multiple_lines() {
            assert_eq!(
                delimited_by_matching_multiples('{', '}')("{{
[s Tadaaa!]
}}"),
                Ok(("", "\n[s Tadaaa!]\n"))
            );
        }

    }
}