use std::io::BufReader;

use serde_json::Value;

#[derive(Debug)]
struct Location {
    start: usize,
    end: usize,
    filename: String
}

#[derive(Debug)]
struct Int {
    value: i32,
    location: Location
}

#[derive(Debug)]
struct Str {
    value: String,
    location: Location
}

#[derive(Debug)]
struct Boolean {
    value: bool,
    location: Location
}

#[derive(Debug)]
struct Tuple {
    first: Term,
    second: Term,
    location: Location
}

#[derive(Debug)]
enum Term {
    Int(Int),
    Str(Str),
    Boolean(Boolean),
    Let(Box<Let>),
    Function(Box<Function>),
    If(Box<If>),
    Binary(Box<Binary>),
    Call(Box<Call>),
    Var(Var),
    Print(Box<Print>),
    First(Box<First>),
    Second(Box<Second>),
    Tuple(Box<Tuple>),
}

#[derive(Debug)]
enum BinaryOperator {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    Eq,
    Neq,
    Lt,
    Gt,
    Lte,
    Gte,
    And,
    Or
}

#[derive(Debug)]
struct Binary {
    lhs: Term,
    rhs: Term,
    op: BinaryOperator,
    location: Location
}

#[derive(Debug)]
struct File {
    name: String,
    expression: Term,
    location: Location
}

#[derive(Debug)]
struct Parameter {
    text: String,
    location: Location
}

#[derive(Debug)]
struct Let {
    name: Parameter,
    value: Term,
    next: Term,
    location: Location,
}

#[derive(Debug)]
struct Function {
    parameters: Vec<Parameter>,
    value: Term,
    location: Location
}

#[derive(Debug)]
struct If {
    condition:Term,
    then: Term,
    otherwise: Term,
    location: Location,
}

#[derive(Debug)]
struct Print {
    value:	Term,
    location: Location,
}

#[derive(Debug)]
struct Call {
    callee:	Term,
    arguments: Vec<Term>,
    location: Location,
}

#[derive(Debug)]
struct Var {
    text: String,
    location: Location,
}

#[derive(Debug)]
struct First {
    value: Term,
    location: Location,
}

#[derive(Debug)]
struct Second {
    value: Term,
    location: Location,
}

fn parse_term(v: &Value) -> Term {
    let kind = get_field("kind", v);
    let kind = as_string(kind);

    match kind.as_str() {
        "Int" => {
            let i = parse_int(v);
            Term::Int(i)
        },
        "Str" => {
            let s = parse_string(v);
            Term::Str(s)
        },
        "Bool" => {
            let s = parse_boolean(v);
            Term::Boolean(s)
        },
        "Let" => {
            let l = parse_let(v);
            Term::Let(Box::new(l))
        },
        "Function" => {
            let f = parse_function(v);
            Term::Function(Box::new(f))
        },
        "If" => {
            let i = parse_if(v);
            Term::If(Box::new(i))
        },
        "Binary" => {
            let b = parse_binary(v);
            Term::Binary(Box::new(b))
        },
        "Call" => {
            let c = parse_call(v);
            Term::Call(Box::new(c))
        },
        "Var" => {
            let v = parse_var(v);
            Term::Var(v)
        },
        "Print" => {
            let p = parse_print(v);
            Term::Print(Box::new(p))
        },
        "Tuple" => {
            let t = parse_tuple(v);
            Term::Tuple(Box::new(t))
        },
        "First" => {
            let f = parse_first(v);
            Term::First(Box::new(f))
        },
        "Second" => {
            let s = parse_second(v);
            Term::Second(Box::new(s))
        },
        _ => panic!("unknown kind: {}", kind)
    }
}

fn parse_tuple(v: &Value) -> Tuple {
    let first = get_field("first", v);
    let second = get_field("second", v);
    let location = get_field("location", v);

    Tuple {
        first: parse_term(first),
        second: parse_term(second),
        location: parse_location(location)
    }
}

fn parse_second(v: &Value) -> Second {
    let value = get_field("value", v);
    let location = get_field("location", v);

    Second {
        value: parse_term(value),
        location: parse_location(location),
    }
}

fn parse_first(v: &Value) -> First {
    let value = get_field("value", v);
    let location = get_field("location", v);

    First {
        value: parse_term(value),
        location: parse_location(location),
    }
}

fn parse_print(v: &Value) -> Print {
    let value = get_field("value", v);
    let location = get_field("location", v);

    Print {
        value: parse_term(value),
        location: parse_location(location)
    }
}

fn parse_var(v: &Value) -> Var {
    let text = get_field("text", v);
    let location = get_field("location", v);

    Var {
        text: as_string(text),
        location: parse_location(location),
    }
}

fn parse_call(v: &Value) -> Call {
    let callee = get_field("callee", v);
    let arguments = get_field("arguments", v);
    let location = get_field("location", v);

    Call {
        callee: parse_term(callee),
        arguments: parse_array(arguments, |t| parse_term(&t)),
        location: parse_location(location),
    }
}

fn parse_binary(v: &Value) -> Binary {
    let lhs = get_field("lhs", v);
    let rhs = get_field("rhs", v);
    let op = get_field("op", v);
    let location = get_field("location", v);

    Binary {
        lhs: parse_term(lhs),
        rhs: parse_term(rhs),
        op: parse_binary_op(as_string(op)),
        location: parse_location(location),
    }
}

fn parse_binary_op(op: String) -> BinaryOperator {
    match op.as_str() {
        "Add" => BinaryOperator::Add,
        "Sub" => BinaryOperator::Sub,
        "Mul" => BinaryOperator::Mul,
        "Div" => BinaryOperator::Div,
        "Rem" => BinaryOperator::Rem,
        "Eq" => BinaryOperator::Eq,
        "Neq" => BinaryOperator::Neq,
        "Lt" => BinaryOperator::Lt,
        "Gt" => BinaryOperator::Gt,
        "Lte" => BinaryOperator::Lte,
        "Gte" => BinaryOperator::Gte,
        "And" => BinaryOperator::And,
        "Or" => BinaryOperator::Or,
        _ => panic!("unknown binary operator: {}", op)
    }
}

fn parse_if(v: &Value) -> If {
    let condition = get_field("condition", v);
    let then = get_field("then", v);
    let otherwise = get_field("otherwise", v);
    let location = get_field("location", v);

    If {
        condition: parse_term(condition),
        then: parse_term(then),
        otherwise: parse_term(otherwise),
        location: parse_location(location),
    }
}

fn parse_function(v: &Value) -> Function {
    let parameters = get_field("parameters", v);
    let value = get_field("value", v);
    let location = get_field("location", v);

    Function {
        parameters: parse_array(parameters, |v| parse_parameter(&v)),
        value: parse_term(value),
        location: parse_location(location),
    }
}

fn parse_let(v: &Value) -> Let {
    let parameter = get_field("name", v);
    let value = get_field("value", v);
    let next = get_field("next", v);
    let location = get_field("location", v);

    Let {
        name: parse_parameter(parameter),
        value: parse_term(value),
        next: parse_term(next),
        location: parse_location(location),
    }
}

fn parse_file(v: &Value) -> File {
    let name =get_field("name", v);
    let expression =get_field("expression", v);
    let location =get_field("location", v);

    File {
        name: as_string(name),
        expression: parse_term(expression),
        location: parse_location(location)
    }
}

fn parse_parameter(v: &Value) -> Parameter {
    let text = get_field("text", v);
    let location = get_field("location", v);

    Parameter {
        text: as_string(text),
        location: parse_location(location)
    }
}

fn parse_location(v: &Value) -> Location {
    let start = get_field("start", v);
    let end = get_field("end", v);
    let filename = get_field("filename", v);

    Location {
        start: as_int(start) as usize,
        end: as_int(end) as usize,
        filename: as_string(filename),
    }
}

fn parse_int(v: &Value) -> Int {
    let value = get_field("value", v);
    let location = get_field("location", v);

    Int {
        value: as_int(value),
        location: parse_location(location)
    }
}

fn parse_string(v: &Value) -> Str {
    let value = get_field("value", v);
    let location = get_field("location", v);

    Str {
        value: as_string(value),
        location: parse_location(location)
    }
}


fn parse_boolean(v: &Value) -> Boolean {
    let value = get_field("value", v);
    let location = get_field("location", v);

    Boolean {
        value: as_bool(value),
        location: parse_location(location)
    }
}

fn parse_array<T, F: Fn(Value) -> T>(v: &Value, f: F) -> Vec<T> {
    let values: Vec<Value> = serde_json::from_value(v.to_owned()).expect("expected array");
    values.into_iter().map(f).collect()
}

fn as_string(v: &Value) -> String {
    serde_json::from_value(v.to_owned()).expect("expected string")
}

fn as_int(v: &Value) -> i32 {
    serde_json::from_value(v.to_owned()).expect("expected integer")
}

fn as_bool(v: &Value) -> bool {
    serde_json::from_value(v.to_owned()).expect("expected boolean")
}

fn get_field<'a>(field_name: &str, v: &'a Value) -> &'a Value {
    v.get(field_name).expect(format!("expected field '{}'", field_name).as_str())
}

fn main() {
    let file = std::fs::File::open("files/fib.json").expect("couldn't open file");
    let buf_reader = BufReader::new(file);
    let v: Value = serde_json::from_reader(buf_reader).expect("failed to read json");
    dbg!(parse_file(&v));
}
