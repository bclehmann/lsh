extern crate pest;
#[macro_use]
extern crate pest_derive;

use std::io;
use std::collections::HashMap;
use std::ops::{Add, Div, Mul, Sub};
use pest::iterators::{Pair, Pairs};
use pest::Parser;
use pest::pratt_parser::{Assoc, Op, PrattParser};
use crate::Value::{Float, Integer};

#[derive(Parser)]
#[grammar = "grammar.pest"]
struct ShellParser;

#[derive(Debug)]
pub enum Value {
    Integer(i64),
    Float(f64),
    String(String),
}

fn is_integer(v: &Value) -> bool {
    return match v {
        Integer(_x) => true,
        _ => false,
    }
}

fn is_string(v: &Value) -> bool {
    return match v {
        Value::String(_s) => true,
        _ => false,
    }
}

fn value_to_float_value(v: Value) -> Value {
    return match v {
        Integer(x) => Float(x as f64),
        Float(_x) => v,
        _ => panic!("Cannot convert string {:?} to float", v),
    };
}

fn get_integral_value(v: Value) -> i64 {
    return match v {
        Integer(x) => x,
        _ => panic!("Cannot coerce non-integral value {:?} to integer", v)
    }
}

fn get_float_value(v: Value) -> f64 {
    return match v {
        Float(x) => x,
        _ => panic!("Cannot coerce non-float value {:?} to float", v)
    }
}

fn value_to_string(v: &Value) -> String {
    return match v {
        Integer(x) => i64::to_string(x),
        Float(x) => f64::to_string(x),
        Value::String(s) => s.clone(),
        _ => panic!("Cannot convert value {:?} to string", v),
    }
}

impl Clone for Value {
    fn clone(&self) -> Self {
        return match self {
            Value::Integer(x) => Value::Integer(*x),
            Value::Float(x) => Value::Float(*x),
            Value::String(s) => Value::String(s.clone()),
            _ => panic!("Asked to clone unknown value {:?}", self)
        }
    }
}

impl Add for Value {
    type Output = Value;

    fn add(self, rhs: Self) -> Self::Output {
        if is_integer(&self) && is_integer(&rhs) {
            return Value::Integer(get_integral_value(self) + get_integral_value(rhs))
        }

        if is_string(&self) || is_string(&rhs) {
            return Value::String(value_to_string(&self) + &value_to_string(&rhs));
        }

        return Value::Float(get_float_value(value_to_float_value(self)) + get_float_value(value_to_float_value(rhs)));
    }
}

impl Sub for Value {
    type Output = Value;

    fn sub(self, rhs: Self) -> Self::Output {
        return match rhs {
            Value::Integer(x) => self + Value::Integer(-x),
            Value::Float(x) => self + Value::Float(-x),
            _ => panic!("Operator- not implemented on values {:?} and {:?}", self, rhs)
        };
    }
}

impl Mul for Value {
    type Output = Value;

    fn mul(self, rhs: Self) -> Self::Output {
        if is_integer(&self) && is_integer(&rhs) {
            return Value::Integer(get_integral_value(self) * get_integral_value(rhs))
        }

        return Value::Float(get_float_value(value_to_float_value(self)) * get_float_value(value_to_float_value(rhs)));
    }
}

impl Div for Value {
    type Output = Value;

    fn div(self, rhs: Self) -> Self::Output {
        if is_integer(&self) && is_integer(&rhs) {
            return Value::Integer(get_integral_value(self) / get_integral_value(rhs))
        }

        return Value::Float(get_float_value(value_to_float_value(self)) / get_float_value(value_to_float_value(rhs)));
    }
}


#[derive(Debug)]
pub enum Expr<'a> {
    Value(Value),
    Identifier(&'a str),
    BinaryOperation(BinaryOperation<'a>)
}

#[derive(Debug)]
pub struct BinaryOperation<'a> {
    left: Box<Expr<'a>>,
    right: Box<Expr<'a>>,
    operator: BinaryOperator
}

#[derive(Debug)]
pub enum BinaryOperator {
    Add,
    Subtract,
    Multiply,
    Divide
}

#[derive(Debug)]
pub struct Assignment<'a> {
    left: &'a str,
    right: Expr<'a>
}

#[derive(Debug)]
pub enum LineOfCode<'a> {
    Expr(Expr<'a>),
    Assignment(Assignment<'a>)
}

fn create_ast(pairs: Pairs<Rule>) -> Vec<LineOfCode> {
    let mut output: Vec<LineOfCode> = Vec::new();
    for pair in pairs {
        output.push(parse_line(pair));
    }

    return output;
}

fn parse_line(pair: Pair<Rule>) -> LineOfCode {
    return match pair.as_rule() {
        Rule::assignment => LineOfCode::Assignment(parse_assignment(pair.into_inner())),
        Rule::expr => LineOfCode::Expr(parse_expr(pair.into_inner())),
        rule => panic!("Unexpected rule {:?}", rule)
    }
}

fn parse_assignment(mut pairs: Pairs<Rule>) -> Assignment {
    let identifier = pairs.next().unwrap_or_else(|| panic!("No identifier in assignment"));
    if let Rule::identifier = identifier.as_rule() {

        let value = pairs.next().unwrap_or_else(|| panic!("No value in assignment"));
        if let Rule::expr = value.as_rule() {
            return Assignment {
                left: identifier.as_str(),
                right: parse_expr(value.into_inner())
            }
        }
    };

    panic!("Malformed assignment");
}

fn parse_expr(pairs: Pairs<Rule>) -> Expr {
    // FWIW, I think prat means something rude in British English
    let pratt = PrattParser::new()
        .op(Op::infix(Rule::PLUS_SIGN, Assoc::Left) | Op::infix(Rule::MINUS_SIGN, Assoc::Left))
        .op(Op::infix(Rule::STAR_SIGN, Assoc::Left) | Op::infix(Rule::FORWARD_SLASH, Assoc::Left));

    pratt
        .map_primary(|p| match p.as_rule() {
            Rule::number => {
                let s = p.as_str();
                if let Ok(as_int) = s.parse::<i64>() {
                    return Expr::Value(Value::Integer(as_int));
                }
                return Expr::Value(Value::Float(s.parse::<f64>().unwrap_or_else(|e| panic!("Could not parse number: {:?}", e))));
            }
            Rule::string => {
                let s = p.as_str();
                let dequoted = &s[1..s.len() - 1];
                return Expr::Value(Value::String(str::to_string(dequoted)));
            },
            Rule::identifier => Expr::Identifier(p.as_str()),
            Rule::expr => parse_expr(p.into_inner()),
            rule => unreachable!("Invalid rule encountered: {:?}", rule)
        })
        .map_infix(|left, op, right| {
            let operator = match op.as_rule() {
                Rule::PLUS_SIGN => BinaryOperator::Add,
                Rule::MINUS_SIGN => BinaryOperator::Subtract,
                Rule::STAR_SIGN => BinaryOperator::Multiply,
                Rule::FORWARD_SLASH => BinaryOperator::Divide,
                rule => unreachable!("Invalid rule encountered: {:?}", rule)
            };

            Expr::BinaryOperation(BinaryOperation {
                    left: Box::new(left),
                    right: Box::new(right),
                    operator
            })
        })
        .parse(pairs)
}

fn interpret_ast<'a>(ast: Vec<LineOfCode<'a>>, context: &mut HashMap<&'a str, Value>) {
    for line in ast {
        interpret_line(&line, context);
    }
}

fn interpret_line<'a,'b>(line: &'b LineOfCode<'a>, context: &mut HashMap<&'a str, Value>) -> Option<Value>
{
    return match line {
        LineOfCode::Assignment(a) => {
            interpret_assignment(a, context);
            None
        },
        LineOfCode::Expr(e) => Some(interpret_expr(e, context))
    };
}

fn interpret_assignment<'a,'b>(line: &'b Assignment<'a>, context: &mut HashMap<&'a str, Value>) {
    context.insert(line.left, interpret_expr(&line.right, context));
}

fn interpret_expr(line: &Expr, context: &HashMap<&str, Value>) -> Value {
    if let Expr::Value(v) = line {
        return v.clone();
    }

    if let Expr::Identifier(id) = line {
        return context.get(*id).unwrap_or_else(|| panic!("Referenced uninitialized variable {:?}", *id)).clone();
    }

    if let Expr::BinaryOperation(op) = line {
        let l = interpret_expr(&*op.left, context);
        let r = interpret_expr(&*op.right, context);

        return match op.operator {
            BinaryOperator::Add => l + r,
            BinaryOperator::Subtract => l - r,
            BinaryOperator::Multiply => l * r,
            BinaryOperator::Divide => l / r,
        };
    };

    unreachable!("Unknown Expr: {:?}", line);
}

fn main() {
    let stdin = io::stdin();
    let input = io::read_to_string(stdin).unwrap_or_else(|e| panic!("Could not read input: {}", e));

    if let Ok(pairs) = ShellParser::parse(Rule::lines, &input) {
        println!("{:?}", pairs);
        let ast = create_ast(pairs);
        println!("{:?}", ast);

        let mut context = HashMap::<&str, Value>::new();
        interpret_ast(ast, &mut context);
        println!("{:?}", context.get("x"));
    } else {
        panic!("Invalid input");
    }
}
