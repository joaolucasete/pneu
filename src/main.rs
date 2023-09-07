use std::io::BufReader;

use serde_json::Value;

#[derive(Debug)]
struct Location {
    start: usize,
    end: usize,
    filename: String
}

#[derive(Debug)]
struct Tuple {
    first: Term,
    second: Term
}

#[derive(Debug)]
struct Term {
    value: TermValue,
    location: Location
}

#[derive(Debug)]
enum TermValue {
    Int(i32),
    Str(String),
    Boolean(bool),
    Var(Var),
    Let(Box<Let>),
    Function(Box<Function>),
    If(Box<If>),
    Binary(Box<Binary>),
    Call(Box<Call>),
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
    op: BinaryOperator
}

#[derive(Debug)]
struct File {
    name: String,
    expression: Term
}

#[derive(Debug)]
struct Parameter(String);

#[derive(Debug)]
struct Let {
    name: Parameter,
    value: Term,
    next: Term
}

#[derive(Debug)]
struct Function {
    parameters: Vec<Parameter>,
    value: Term
}

#[derive(Debug)]
struct If {
    condition:Term,
    then: Term,
    otherwise: Term
}

#[derive(Debug)]
struct Call {
    callee:	Term,
    arguments: Vec<Term>
}

#[derive(Debug)]
struct Var(String);

#[derive(Debug)]
struct First(Term);

#[derive(Debug)]
struct Second(Term);

#[derive(Debug)]
struct Print(Term);

fn parse_term(v: &Value) -> Term {
    let location = get_field("location", v);
    let kind = get_field("kind", v);
    let kind = as_string(kind);

    let value = match kind.as_str() {
        "Int" => {
            let i = parse_int(v);
            TermValue::Int(i)
        },
        "Str" => {
            let s = parse_string(v);
            TermValue::Str(s)
        },
        "Bool" => {
            let s = parse_boolean(v);
            TermValue::Boolean(s)
        },
        "Let" => {
            let l = parse_let(v);
            TermValue::Let(Box::new(l))
        },
        "Function" => {
            let f = parse_function(v);
            TermValue::Function(Box::new(f))
        },
        "If" => {
            let i = parse_if(v);
            TermValue::If(Box::new(i))
        },
        "Binary" => {
            let b = parse_binary(v);
            TermValue::Binary(Box::new(b))
        },
        "Call" => {
            let c = parse_call(v);
            TermValue::Call(Box::new(c))
        },
        "Var" => {
            let v = parse_var(v);
            TermValue::Var(v)
        },
        "Print" => {
            let p = parse_print(v);
            TermValue::Print(Box::new(p))
        },
        "Tuple" => {
            let t = parse_tuple(v);
            TermValue::Tuple(Box::new(t))
        },
        "First" => {
            let f = parse_first(v);
            TermValue::First(Box::new(f))
        },
        "Second" => {
            let s = parse_second(v);
            TermValue::Second(Box::new(s))
        },
        _ => panic!("unknown kind: {}", kind)
    };

    Term {
        value,
        location: parse_location(location)
    }
}

fn parse_tuple(v: &Value) -> Tuple {
    let first = get_field("first", v);
    let second = get_field("second", v);

    Tuple {
        first: parse_term(first),
        second: parse_term(second)
    }
}

fn eval(t: TermValue) -> TermValue {
    match t {
        TermValue::Int(_) => t,
        TermValue::Str(_) => t,
        TermValue::Boolean(_) => t,
        TermValue::Let(_) => todo!(),
        TermValue::Function(_) => todo!(),
        TermValue::If(_) => todo!(),
        TermValue::Binary(b) => eval_binary(b.as_ref()),
        TermValue::Call(_) => todo!(),
        TermValue::Var(_) => todo!(),
        TermValue::Print(_) => todo!(),
        TermValue::First(_) => todo!(),
        TermValue::Second(_) => todo!(),
        TermValue::Tuple(_) => todo!(),
    }
}

fn eval_binary(b: &Binary) -> TermValue {
    match &b.op {
        BinaryOperator::Add => add_operation(b),
        BinaryOperator::Sub => todo!(),
        BinaryOperator::Mul => todo!(),
        BinaryOperator::Div => todo!(),
        BinaryOperator::Rem => todo!(),
        BinaryOperator::Eq => todo!(),
        BinaryOperator::Neq => todo!(),
        BinaryOperator::Lt => todo!(),
        BinaryOperator::Gt => todo!(),
        BinaryOperator::Lte => todo!(),
        BinaryOperator::Gte => todo!(),
        BinaryOperator::And => todo!(),
        BinaryOperator::Or => todo!(),
    }
}

fn add_operation(b: &Binary) -> TermValue {

    match (&b.rhs.value, &b.lhs.value) {
        (TermValue::Int(l), TermValue::Int(r)) => TermValue::Int(l + r),
        _ => todo!()
    }
}

fn parse_second(v: &Value) -> Second {
    let value = get_field("value", v);

    Second(parse_term(value))
}

fn parse_first(v: &Value) -> First {
    let value = get_field("value", v);

    First(parse_term(value))
}

fn parse_print(v: &Value) -> Print {
    let value = get_field("value", v);

    Print(parse_term(value))
}

fn parse_var(v: &Value) -> Var {
    let text = get_field("text", v);

    Var(as_string(text))
}

fn parse_call(v: &Value) -> Call {
    let callee = get_field("callee", v);
    let arguments = get_field("arguments", v);

    Call {
        callee: parse_term(callee),
        arguments: parse_array(arguments, |t| parse_term(&t)),
    }
}

fn parse_binary(v: &Value) -> Binary {
    let lhs = get_field("lhs", v);
    let rhs = get_field("rhs", v);
    let op = get_field("op", v);

    Binary {
        lhs: parse_term(lhs),
        rhs: parse_term(rhs),
        op: parse_binary_op(as_string(op)),
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

    If {
        condition: parse_term(condition),
        then: parse_term(then),
        otherwise: parse_term(otherwise),
    }
}

fn parse_function(v: &Value) -> Function {
    let parameters = get_field("parameters", v);
    let value = get_field("value", v);

    Function {
        parameters: parse_array(parameters, |v| parse_parameter(&v)),
        value: parse_term(value),
    }
}

fn parse_let(v: &Value) -> Let {
    let parameter = get_field("name", v);
    let value = get_field("value", v);
    let next = get_field("next", v);

    Let {
        name: parse_parameter(parameter),
        value: parse_term(value),
        next: parse_term(next),
    }
}

fn parse_file(v: &Value) -> File {
    let name =get_field("name", v);
    let expression =get_field("expression", v);

    File {
        name: as_string(name),
        expression: parse_term(expression),
    }
}

fn parse_parameter(v: &Value) -> Parameter {
    let text = get_field("text", v);

    Parameter(as_string(text))
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

fn parse_int(v: &Value) -> i32 {
    let value = get_field("value", v);
    as_int(value)
}

fn parse_string(v: &Value) -> String {
    let value = get_field("value", v);
    as_string(value)
}


fn parse_boolean(v: &Value) -> bool {
    let value = get_field("value", v);
    as_bool(value)
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

    let binary = Binary {
        lhs: Term {
            value: TermValue::Int(32),
            location: sample_location()
        },
        rhs: Term {
            value: TermValue::Int(32),
            location: sample_location()
        },
        op: BinaryOperator::Add
    };
    dbg!(eval_binary(&binary));
}


fn sample_location() -> Location {
    Location { start: 1, end: 1, filename: "sample-location".to_owned() }
}