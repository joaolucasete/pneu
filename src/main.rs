use std::{io::BufReader, ops::Add};

use serde_json::Value;

// Define a macro that takes two expressions and an operator as arguments
macro_rules! binary {
    ($lhs:expr, $op:expr, $rhs:expr) => {
        // Create a Binary struct with the given arguments
        Binary {
            lhs: Term {
                value: $lhs,
                location: Location {
                    start: 1,
                    end: 1,
                    filename: "sample-location".into(),
                }
            },
            rhs: Term {
                value: $rhs,
                location: Location {
                    start: 1,
                    end: 1,
                    filename: "sample-location".into(),
                }
            },
            op: $op,
        }
    };
}

#[derive(Debug, Eq, PartialEq)]
struct Location {
    start: usize,
    end: usize,
    filename: String,
}

#[derive(Debug, Eq, PartialEq)]
struct Tuple {
    first: Term,
    second: Term,
}

#[derive(Debug, Eq, PartialEq)]
struct Term {
    value: TermValue,
    location: Location,
}

#[derive(Debug, PartialEq, Eq)]
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

#[derive(Debug, Eq, PartialEq)]
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
    Or,
}

#[derive(Debug, Eq, PartialEq)]
struct Binary {
    lhs: Term,
    rhs: Term,
    op: BinaryOperator,
}

#[derive(Debug, Eq, PartialEq)]
struct File {
    name: String,
    expression: Term,
}

#[derive(Debug, Eq, PartialEq)]
struct Parameter(String);

#[derive(Debug, Eq, PartialEq)]
struct Let {
    name: Parameter,
    value: Term,
    next: Term,
}

#[derive(Debug, Eq, PartialEq)]
struct Function {
    parameters: Vec<Parameter>,
    value: Term,
}

#[derive(Debug, Eq, PartialEq)]
struct If {
    condition: Term,
    then: Term,
    otherwise: Term,
}

#[derive(Debug, Eq, PartialEq)]
struct Call {
    callee: Term,
    arguments: Vec<Term>,
}

#[derive(Debug, Eq, PartialEq)]
struct Var(String);

#[derive(Debug, Eq, PartialEq)]
struct First(Term);

#[derive(Debug, Eq, PartialEq)]
struct Second(Term);

#[derive(Debug, Eq, PartialEq)]
struct Print(Term);

fn parse_term(v: &Value) -> Term {
    let location = get_field("location", v);
    let kind = get_field("kind", v);
    let kind = as_string(kind);

    let value = match kind.as_str() {
        "Int" => {
            let i = parse_int(v);
            TermValue::Int(i)
        }
        "Str" => {
            let s = parse_string(v);
            TermValue::Str(s)
        }
        "Bool" => {
            let s = parse_boolean(v);
            TermValue::Boolean(s)
        }
        "Let" => {
            let l = parse_let(v);
            TermValue::Let(Box::new(l))
        }
        "Function" => {
            let f = parse_function(v);
            TermValue::Function(Box::new(f))
        }
        "If" => {
            let i = parse_if(v);
            TermValue::If(Box::new(i))
        }
        "Binary" => {
            let b = parse_binary(v);
            TermValue::Binary(Box::new(b))
        }
        "Call" => {
            let c = parse_call(v);
            TermValue::Call(Box::new(c))
        }
        "Var" => {
            let v = parse_var(v);
            TermValue::Var(v)
        }
        "Print" => {
            let p = parse_print(v);
            TermValue::Print(Box::new(p))
        }
        "Tuple" => {
            let t = parse_tuple(v);
            TermValue::Tuple(Box::new(t))
        }
        "First" => {
            let f = parse_first(v);
            TermValue::First(Box::new(f))
        }
        "Second" => {
            let s = parse_second(v);
            TermValue::Second(Box::new(s))
        }
        _ => panic!("unknown kind: {}", kind),
    };

    Term {
        value,
        location: parse_location(location),
    }
}

fn parse_tuple(v: &Value) -> Tuple {
    let first = get_field("first", v);
    let second = get_field("second", v);

    Tuple {
        first: parse_term(first),
        second: parse_term(second),
    }
}

fn eval(t: TermValue) -> TermValue {
    match t {
        TermValue::Int(_) => t,
        TermValue::Str(_) => t,
        TermValue::Boolean(_) => t,
        TermValue::Let(_) => todo!(),
        TermValue::Function(_) => todo!(),
        TermValue::If(i) => eval_if(i),
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
        BinaryOperator::Sub => sub_operation(b),
        BinaryOperator::Mul => mul_operation(b),
        BinaryOperator::Div => div_operation(b),
        BinaryOperator::Rem => rem_operation(b),
        BinaryOperator::Eq => eq_operation(b),
        BinaryOperator::Neq => neq_operation(b),
        BinaryOperator::Lt => lt_operation(b),
        BinaryOperator::Gt => gt_operation(b),
        BinaryOperator::Lte => lte_operation(b),
        BinaryOperator::Gte => gte_operation(b),
        BinaryOperator::And => and_operation(b),
        BinaryOperator::Or => or_operation(b),
    }
}

fn or_operation(b: &Binary) -> TermValue {
    match (&b.lhs.value, &b.rhs.value) {
        (TermValue::Boolean(l), TermValue::Boolean(r)) => TermValue::Boolean(*l || *r),
        _ => panic!("invalid operation between types"),
    }
}

fn and_operation(b: &Binary) -> TermValue {
    match (&b.lhs.value, &b.rhs.value) {
        (TermValue::Boolean(l), TermValue::Boolean(r)) => TermValue::Boolean(*l && *r),
        _ => panic!("invalid operation between types"),
    }
}

fn gt_operation(b: &Binary) -> TermValue {
    match (&b.lhs.value, &b.rhs.value) {
        (TermValue::Int(l), TermValue::Int(r)) => TermValue::Boolean(l > r),
        _ => panic!("invalid operation between types"),
    }
}

fn gte_operation(b: &Binary) -> TermValue {
    match (&b.lhs.value, &b.rhs.value) {
        (TermValue::Int(l), TermValue::Int(r)) => TermValue::Boolean(l >= r),
        _ => panic!("invalid operation between types"),
    }
}

fn lt_operation(b: &Binary) -> TermValue {
    match (&b.lhs.value, &b.rhs.value) {
        (TermValue::Int(l), TermValue::Int(r)) => TermValue::Boolean(l < r),
        _ => panic!("invalid operation between types"),
    }
}

fn lte_operation(b: &Binary) -> TermValue {
    match (&b.lhs.value, &b.rhs.value) {
        (TermValue::Int(l), TermValue::Int(r)) => TermValue::Boolean(l <= r),
        _ => panic!("invalid operation between types"),
    }
}

fn neq_operation(b: &Binary) -> TermValue {
    TermValue::Boolean(b.lhs != b.rhs)
}

fn eq_operation(b: &Binary) -> TermValue {
    TermValue::Boolean(b.rhs == b.lhs)
}

fn rem_operation(b: &Binary) -> TermValue {
    match (&b.lhs.value, &b.rhs.value) {
        (TermValue::Int(l), TermValue::Int(r)) => TermValue::Int(l % r),
        _ => panic!("invalid operation between types"),
    }
}

fn div_operation(b: &Binary) -> TermValue {
    match (&b.lhs.value, &b.rhs.value) {
        (TermValue::Int(l), TermValue::Int(r)) => TermValue::Int(l / r),
        _ => panic!("invalid operation between types"),
    }
}

fn mul_operation(b: &Binary) -> TermValue {
    match (&b.lhs.value, &b.rhs.value) {
        (TermValue::Int(l), TermValue::Int(r)) => TermValue::Int(l * r),
        _ => panic!("invalid operation between types"),
    }
}

fn sub_operation(b: &Binary) -> TermValue {
    match (&b.lhs.value, &b.rhs.value) {
        (TermValue::Int(l), TermValue::Int(r)) => TermValue::Int(l - r),
        _ => panic!("invalid operation between types"),
    }
}

fn add_operation(b: &Binary) -> TermValue {
    match (&b.lhs.value, &b.rhs.value) {
        (TermValue::Int(l), TermValue::Int(r)) => TermValue::Int(l + r),
        (TermValue::Str(l), TermValue::Str(r)) => TermValue::Str(format!("{}{}", l, r)),
        (TermValue::Str(l), TermValue::Int(r)) => TermValue::Str(format!("{}{}", l, r)),
        (TermValue::Int(l), TermValue::Str(r)) => TermValue::Str(format!("{}{}", l, r)),
        (_, _) => panic!("invalid operation between types"),
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
        _ => panic!("unknown binary operator: {}", op),
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
    let name = get_field("name", v);
    let expression = get_field("expression", v);

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
    v.get(field_name)
        .expect(format!("expected field '{}'", field_name).as_str())
}

fn main() {
    let file = std::fs::File::open("files/fib.json").expect("couldn't open file");
    let buf_reader = BufReader::new(file);
    let v: Value = serde_json::from_reader(buf_reader).expect("failed to read json");

    println!("hello world")
    // dbg!(eval_binary(&binary));
}

#[cfg(test)]
mod tests {
    use super::*;

    // TODO: write macro for simplifying expr creation in binary expr tests
    #[test]
    fn test_add_int_int() {
        let expr = binary!(TermValue::Int(32), BinaryOperator::Add, TermValue::Int(32));
        let result = add_operation(&expr);
        assert_eq!(result, TermValue::Int(64));
    }

    #[test]
    fn test_add_int_str() {
        let expr = binary!(TermValue::Int(32), BinaryOperator::Add, TermValue::Str("64".into()));
        let result = eval_binary(&expr);
        assert_eq!(result, TermValue::Str("3264".into()));
    }

    #[test]
    fn test_add_str_int() {
        let expr = binary!(TermValue::Str("64".into()), BinaryOperator::Add, TermValue::Int(32));
        let result = eval_binary(&expr);
        assert_eq!(result, TermValue::Str("6432".into()));
    }

    #[test]
    fn test_add_str_str() {
        let expr = binary!(TermValue::Str("32".into()), BinaryOperator::Add, TermValue::Str("64".into()));
        let result = eval_binary(&expr);
        assert_eq!(result, TermValue::Str("3264".into()));
    }

    // -------------------- EQ -----------------------

    #[test]
    fn test_eq_str_str() {
        let expr = binary!(TermValue::Str("igualstrings".into()), BinaryOperator::Eq, TermValue::Str("igualstrings".into()));
        let result = eval_binary(&expr);
        assert_eq!(result, TermValue::Boolean(true));

        let expr = binary!(TermValue::Str("diffstrings".into()), BinaryOperator::Eq, TermValue::Str("igualstrings".into()));
        let result = eval_binary(&expr);
        assert_eq!(result, TermValue::Boolean(false));
    }

    #[test]
    fn test_eq_int_int() {
        let expr = binary!(TermValue::Int(59), BinaryOperator::Eq, TermValue::Int(59));
        let result = eval_binary(&expr);
        assert_eq!(result, TermValue::Boolean(true));

        let expr = binary!(TermValue::Int(59), BinaryOperator::Eq, TermValue::Int(80));
        let result = eval_binary(&expr);
        assert_eq!(result, TermValue::Boolean(false));
    }

    #[test]
    fn test_eq_bool_bool() {
        let expr = binary!(TermValue::Boolean(true), BinaryOperator::Eq, TermValue::Boolean(true));
        let result = eval_binary(&expr);
        assert_eq!(result, TermValue::Boolean(true));

        let expr = binary!(TermValue::Boolean(true), BinaryOperator::Eq, TermValue::Boolean(false));
        let result = eval_binary(&expr);
        assert_eq!(result, TermValue::Boolean(false));
    }

    #[test]
    fn test_eq_int_bool() {
        let expr = binary!(TermValue::Int(15), BinaryOperator::Eq, TermValue::Boolean(true));
        let result = eval_binary(&expr);
        assert_eq!(result, TermValue::Boolean(false));
    }

    // -----------------------------------------------
    #[test]
    fn test_and_bool() {
        let expr = binary!(TermValue::Boolean(true), BinaryOperator::And, TermValue::Boolean(false));
        let result = eval_binary(&expr);
        assert_eq!(result, TermValue::Boolean(false));

        let expr = binary!(TermValue::Boolean(true), BinaryOperator::And, TermValue::Boolean(true));
        let result = eval_binary(&expr);
        assert_eq!(result, TermValue::Boolean(true));
    }

    // -----------------------------------------------

    #[test]
    fn test_or_bool() {
        let expr = binary!(TermValue::Boolean(true), BinaryOperator::Or, TermValue::Boolean(false));
        let result = eval_binary(&expr);
        assert_eq!(result, TermValue::Boolean(true));

        let expr = binary!(TermValue::Boolean(false), BinaryOperator::Or, TermValue::Boolean(false));
        let result = eval_binary(&expr);
        assert_eq!(result, TermValue::Boolean(false));
    }

    // -----------------------------------------------

    #[test]
    fn test_gt_int() {
        let expr = binary!(TermValue::Int(32), BinaryOperator::Gt, TermValue::Int(24));
        let result = eval_binary(&expr);
        assert_eq!(result, TermValue::Boolean(true));

        let expr = binary!(TermValue::Int(32), BinaryOperator::Gt, TermValue::Int(64));
        let result = eval_binary(&expr);
        assert_eq!(result, TermValue::Boolean(false));
    }

    // -----------------------------------------------

    #[test]
    fn test_lt_int() {
        let expr = binary!(TermValue::Int(64), BinaryOperator::Lt, TermValue::Int(32));
        let result = eval_binary(&expr);
        assert_eq!(result, TermValue::Boolean(false));

        let expr = binary!(TermValue::Int(22), BinaryOperator::Lt, TermValue::Int(32));
        let result = eval_binary(&expr);
        assert_eq!(result, TermValue::Boolean(true));
    }

    // -----------------------------------------------

    #[test]
    fn test_lte_int() {
        let expr = binary!(TermValue::Int(64), BinaryOperator::Lte, TermValue::Int(64));
        let result = eval_binary(&expr);
        assert_eq!(result, TermValue::Boolean(true));

        let expr = binary!(TermValue::Int(65), BinaryOperator::Lte, TermValue::Int(64));
        let result = eval_binary(&expr);
        assert_eq!(result, TermValue::Boolean(false));
    }

    // -----------------------------------------------

    #[test]
    fn test_gte_int() {
        let expr = binary!(TermValue::Int(64), BinaryOperator::Gte, TermValue::Int(64));
        let result = eval_binary(&expr);
        assert_eq!(result, TermValue::Boolean(true));

        let expr = binary!(TermValue::Int(10), BinaryOperator::Gte, TermValue::Int(5));
        let result = eval_binary(&expr);
        assert_eq!(result, TermValue::Boolean(true));
    }

    // -----------------------------------------------
    #[test]
    fn test_mul_int() {
        let expr = binary!(TermValue::Int(64), BinaryOperator::Mul, TermValue::Int(32));
        let result = eval_binary(&expr);
        assert_eq!(result, TermValue::Int(2048));
    }

    // ------------------------------------------------

    #[test]
    fn test_div_int() {
        let expr = binary!(TermValue::Int(64), BinaryOperator::Div, TermValue::Int(2));
        let result = eval_binary(&expr);
        assert_eq!(result, TermValue::Int(32));
    }

    // ------------------------------------------------

    #[test]
    fn test_rem_int() {
        let expr = binary!(TermValue::Int(64), BinaryOperator::Rem, TermValue::Int(2));
        let result = eval_binary(&expr);
        assert_eq!(result, TermValue::Int(0));

        let expr = binary!(TermValue::Int(3), BinaryOperator::Rem, TermValue::Int(2));
        let result = eval_binary(&expr);
        assert_eq!(result, TermValue::Int(1));
    }

    // ------------------------------------------------

    #[test]
    fn test_neq_int() {
        let expr = binary!(TermValue::Int(64), BinaryOperator::Neq, TermValue::Int(2));
        let result = eval_binary(&expr);
        assert_eq!(result, TermValue::Boolean(true));
    }

    #[test]
    fn test_neq_str() {
        let expr = binary!(TermValue::Str("alou".into()), BinaryOperator::Neq, TermValue::Str("alou".into()));
        let result = eval_binary(&expr);
        assert_eq!(result, TermValue::Boolean(false));

        let expr = binary!(TermValue::Str("alou".into()), BinaryOperator::Neq, TermValue::Str("alo".into()));
        let result = eval_binary(&expr);
        assert_eq!(result, TermValue::Boolean(true));
    }

    #[test]
    fn test_neq_bool() {
        let expr = binary!(TermValue::Boolean(true), BinaryOperator::Neq, TermValue::Boolean(false));
        let result = eval_binary(&expr);
        assert_eq!(result, TermValue::Boolean(true));
    }

    // ------------------------------------------------

    #[test]
    fn test_sub_int() {
        let expr = binary!(TermValue::Int(64), BinaryOperator::Sub, TermValue::Int(2));
        let result = eval_binary(&expr);
        assert_eq!(result, TermValue::Int(62));

        let expr = binary!(TermValue::Int(32), BinaryOperator::Sub, TermValue::Int(64));
        let result = eval_binary(&expr);
        assert_eq!(result, TermValue::Int(-32));
    }
}
