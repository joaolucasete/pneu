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
enum Term {
    Int(Int),
    Str(Str),
    Boolean(Boolean),
    Let(Box<Let>),
    Function(Box<Function>),
    If(Box<If>),
    Binary(Box<Binary>),
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

// TODO: File

fn parse_term(v: &Value) -> Term {
    let kind = v.get("kind").expect("expected string");
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
        _ => {
            let b = Boolean {
                value: false,
                location: Location {
                    start: 1,
                    end: 1,
                    filename: "a".to_owned()
                }
            };

            Term::Boolean(b)
        }
    }
}

fn parse_binary(v: &Value) -> Binary {
    let lhs = v.get("lhs").expect("expected lhs");
    let rhs = v.get("rhs").expect("expected rhs");
    let op = v.get("op").expect("expected op");
    let location = v.get("location").expect("expected location");

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
    let condition = v.get("condition").expect("expected condition");
    let then = v.get("then").expect("expected then");
    let otherwise = v.get("otherwise").expect("expected otherwise");
    let location = v.get("location").expect("expected location");

    If {
        condition: parse_term(condition),
        then: parse_term(then),
        otherwise: parse_term(otherwise),
        location: parse_location(location),
    }
}

fn parse_function(v: &Value) -> Function {
    let parameters = v.get("parameters").expect("expected parameters");
    let value = v.get("value").expect("expected value");
    let location = v.get("location").expect("expected location");

    Function {
        parameters: parse_array(parameters, |v| parse_parameter(&v)),
        value: parse_term(value),
        location: parse_location(location),
    }
}

fn parse_let(v: &Value) -> Let {
    let parameter = v.get("name").expect("expected name");
    let value = v.get("value").expect("expected value");
    let next = v.get("next").expect("expected next");
    let location = v.get("location").expect("expected location");

    Let {
        name: parse_parameter(parameter),
        value: parse_term(value),
        next: parse_term(next),
        location: parse_location(location),
    }
}

fn parse_file(v: Value) -> File {
    let name = v.get("name").expect("expected name");
    let expression = v.get("expression").expect("expected expression");
    let location = v.get("location").expect("expected location");

    File {
        name: as_string(name),
        expression: parse_term(expression),
        location: parse_location(location)
    }
}

fn parse_parameter(v: &Value) -> Parameter {
    let text = v.get("text").expect("expected text");
    let location = v.get("location").expect("expected location");

    Parameter {
        text: as_string(text),
        location: parse_location(location)
    }
}

fn parse_location(v: &Value) -> Location {
    let start = v.get("start").expect("expected start");
    let end = v.get("end").expect("expected end");
    let filename = v.get("filename").expect("expected filename");

    Location {
        start: as_int(start) as usize,
        end: as_int(end) as usize,
        filename: as_string(filename),
    }
}

fn parse_int(v: &Value) -> Int {
    let value = v.get("value").expect("expected value");
    let location = v.get("location").expect("expected location");

    Int {
        value: as_int(value),
        location: parse_location(location)
    }
}

fn parse_string(v: &Value) -> Str {
    let value = v.get("value").expect("expected value");
    let location = v.get("location").expect("expected location");

    Str {
        value: as_string(value),
        location: parse_location(location)
    }
}


fn parse_boolean(v: &Value) -> Boolean {
    let value = v.get("value").expect("expected value");
    let location = v.get("location").expect("expected location");

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

fn main() {
    let file = std::fs::File::open("files/fib.json").expect("couldn't open file");
    let buf_reader = BufReader::new(file);
    let v: Value = serde_json::from_reader(buf_reader).expect("failed to read json");
    // parse_json_ast(&v);
    dbg!(parse_file(v));

}
