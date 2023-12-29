extern crate pest;
#[macro_use]
extern crate pest_derive;

use std::io;
use std::collections::HashMap;
use std::fmt::{Debug, Formatter};
use std::ops::{Add, Div, Mul, Sub};
use std::rc::Rc;
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
    None
}

impl Value {
    fn is_truthy(&self) -> bool {
        match self {
            Value::Integer(x) => *x != 0,
            Value::Float(x) => *x != 0f64,
            Value::String(s) => s != "",
            Value::None => false,
            _ => panic!("Unknown value")
        }
    }

    fn format(&self) -> String {
        match self {
            Value::Integer(x) => x.to_string(),
            Value::Float(x) => x.to_string(),
            Value::String(s) => s.to_string(),
            Value::None => "None".to_string(),
            _ => panic!("Unknown value"),
        }
    }
}

impl PartialEq<Self> for Value {
    fn eq(&self, other: &Self) -> bool {
        match self {
            Value::Integer(x) => {
                return match other {
                    Value::Integer(y) => return x == y,
                    Value::Float(y) => return *x as f64 == *y,
                    _ => false,
                }
            },
            Value::Float(x) => {
                return match other {
                    Value::Integer(y) => return *x == *y as f64,
                    Value::Float(y) => return x == y,
                    _ => false,
                }
            },
            Value::String(s) => {
                if let Value::String(s2) = other {
                    return s == s2;
                }
                return false;
            },
            Value::None => {
                return match other {
                    Value::None => true,
                    _ => false
                }
            },
            _ => panic!("Unknown value")
        }
    }
}

impl Eq for Value {

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
    BinaryOperation(BinaryOperation<'a>),
    FunctionCall(FunctionCall<'a>),
    Block(Vec<LineOfCode<'a>>),
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
    Divide,
    LogicalAnd,
    LogicalOr,
    Equals,
    NotEqual
}

#[derive(Debug)]
pub struct Assignment<'a> {
    left: &'a str,
    right: Expr<'a>
}

#[derive(Debug)]
pub struct FunctionCall<'a> {
    function_name: &'a str,
    arguments: Vec<Expr<'a>>
}

#[derive(Debug)]
pub enum LineOfCode<'a> {
    Expr(Expr<'a>),
    Assignment(Assignment<'a>),
    FunctionDefinition(String, Function<'a>),
    ReturnStatement(Expr<'a>),
    IfStatement(IfStatement<'a>)
}

#[derive(Debug)]
pub struct IfStatement<'a> {
    condition: Expr<'a>,
    body: Expr<'a>,
    else_body: Option<Box<LineOfCode<'a>>>
}

#[derive(Debug)]
pub struct Function<'a> {
    parameters: Rc<Vec<&'a str>>,
    body: Rc<FunctionBody<'a>>,
}

impl<'a> Clone for Function<'a> {
    fn clone(&self) -> Self {
        return Self {
            parameters: self.parameters.clone(),
            body: self.body.clone()
        }
    }
}

#[derive(Debug)]
pub enum FunctionBody<'a> {
    Expr(Expr<'a>),
    Intrinsic(String)
}

pub struct ExecutionContext<'a> {
    parent_scope: Option<&'a mut ExecutionContext<'a>>,
    variables: HashMap<String, Value>,
    functions: HashMap<String, Function<'a>>,
}

impl<'a> ExecutionContext<'a> {
    fn new() -> Self {
        Self {
            parent_scope: None,
            variables: HashMap::<String, Value>::new(),
            functions: HashMap::<String, Function>::new(),
        }
    }

    fn get_value(&self, identifier: &str) -> Option<Value> {
        let local = self.variables.get(identifier);
        if let Some(v) = local {
            return Some(v.clone());
        }

        if let Some(p) = &self.parent_scope {
            if let Some(v) = p.get_value(identifier) {
                return Some(v.clone());
            }
        }

        return None;
    }

    fn get_function(&self, identifier: &str) -> Option<&Function> {
        let local = self.functions.get(identifier);
        if let Some(f) = local {
            return Some(f);
        }

        if let Some(p) = &self.parent_scope {
            if let Some(f) = p.get_function(identifier) {
                return Some(f);
            }
        }

        return None;
    }

    fn update_value_if_present<'b>(&mut self, identifier: &'b str, v: Value) -> bool where 'b : 'a {
        if self.variables.contains_key(identifier) {
            self.variables.insert(identifier.to_string(), v);
            return true;
        }

        if let Some(p) = &mut self.parent_scope {
            p.update_value_if_present(identifier, v);
        }

        return false;
    }

    fn set_value_do_not_traverse_parents<'b>(&mut self, identifier: &'b str, v: Value) where 'b : 'a {
        self.variables.insert(identifier.to_string(), v.clone());
    }

    fn set_value<'b>(&mut self, identifier: &'b str, v: Value) where 'b : 'a {
        if !self.update_value_if_present(identifier, v.clone()) {
            self.set_value_do_not_traverse_parents(identifier, v.clone());
        }
    }
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
        Rule::function_def => {
            let (identifier, function) = parse_function_def(pair.into_inner());
            return LineOfCode::FunctionDefinition(identifier, function);
        },
        Rule::return_statement => LineOfCode::ReturnStatement(parse_expr(pair.into_inner())),
        Rule::if_statement => LineOfCode::IfStatement(parse_if_statement(pair.into_inner())),
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
        .op(Op::infix(Rule::STAR_SIGN, Assoc::Left) | Op::infix(Rule::FORWARD_SLASH, Assoc::Left))
        .op(Op::infix(Rule::EQUALS_OPERATOR, Assoc::Left) | Op::infix(Rule::NOT_EQUALS_OPERATOR, Assoc::Left))
        .op(Op::infix(Rule::DOUBLE_AMP, Assoc::Left) | Op::infix(Rule::DOUBLE_PIPE, Assoc::Left));

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
            Rule::function_call => Expr::FunctionCall(parse_function_call(p.into_inner())),
            Rule::block => Expr::Block(create_ast(p.into_inner())),
            rule => unreachable!("Invalid rule encountered: {:?}", rule)
        })
        .map_infix(|left, op, right| {
            let operator = match op.as_rule() {
                Rule::PLUS_SIGN => BinaryOperator::Add,
                Rule::MINUS_SIGN => BinaryOperator::Subtract,
                Rule::STAR_SIGN => BinaryOperator::Multiply,
                Rule::FORWARD_SLASH => BinaryOperator::Divide,
                Rule::DOUBLE_AMP => BinaryOperator::LogicalAnd,
                Rule::DOUBLE_PIPE => BinaryOperator::LogicalOr,
                Rule::EQUALS_OPERATOR => BinaryOperator::Equals,
                Rule::NOT_EQUALS_OPERATOR => BinaryOperator::NotEqual,
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

fn parse_function_def(mut pairs: Pairs<Rule>) -> (String, Function) {
    let identifier = pairs.next().unwrap_or_else(|| panic!("No function name"));
    let param_list = pairs.next().unwrap_or_else(|| panic!("No parameter list"));

    let mut parameters = Vec::<&str>::new();
    for param in param_list.into_inner() {
        parameters.push(param.as_str());
    }

    let body = parse_expr(pairs.next().unwrap_or_else(|| panic!("No function body")).into_inner());

    return (identifier.as_str().to_string(), Function {
        parameters: Rc::new(parameters),
        body: Rc::new(FunctionBody::Expr(body))
    });
}

fn parse_function_call(mut pairs: Pairs<Rule>) -> FunctionCall {
    let identifier = pairs.next().unwrap_or_else(|| panic!("No identifier in assignment"));
    let mut args = Vec::<Expr>::new();

    if let Rule::identifier = identifier.as_rule() {
        for expr in pairs {
            args.push(parse_expr(expr.into_inner()))
        }

        return FunctionCall {
            function_name: identifier.as_str(),
            arguments: args
        };
    };

    panic!("Malformed function call");
}

fn parse_if_statement(mut pairs: Pairs<Rule>) -> IfStatement {
    let condition = parse_expr(pairs.next().unwrap_or_else(|| panic!("No condition in if statement")).into_inner());
    let body = parse_expr(pairs.next().unwrap_or_else(|| panic!("No body in if statement")).into_inner());

    if let Some(else_body) = pairs.next() {
        return IfStatement {
            condition,
            body,
            else_body: Some(Box::new(parse_line(else_body)))
        };
    }
    
    return IfStatement {
        condition,
        body,
        else_body: None,
    };
}

fn interpret_ast<'a>(ast: &Vec<LineOfCode<'a>>, mut context: Box<ExecutionContext<'a>>) -> (Box<ExecutionContext<'a>>, Option<Value>) {
    for line in ast {
        let (result, c) = interpret_line(&line, context);
        context = c;

        if let LineOfCode::ReturnStatement(_) = line {
            return (context, result);
        }
    }

    return (context, None);
}

fn interpret_line<'a,'b>(line: &'b LineOfCode<'a>, mut context: Box<ExecutionContext<'a>>) -> (Option<Value>, Box<ExecutionContext<'a>>)
{
    return match line {
        LineOfCode::Assignment(a) => {
            let c = interpret_assignment(a, context);
            return (None, c);
        },
        LineOfCode::Expr(e) | LineOfCode::ReturnStatement(e) => {
            let result = interpret_expr(e, &*context);
            return (Some(result), context);
        },
        LineOfCode::FunctionDefinition(name, f) => {
            context.functions.insert(name.clone(), f.clone());
            return (None, context);
        },
        LineOfCode::IfStatement(statement) => {
            if (interpret_expr(&statement.condition, &*context).is_truthy()) {
                let result = interpret_expr(&statement.body, &*context);
                return (Some(result), context);
            } else if let Some(e) = &statement.else_body {
                return interpret_line(&*e, context);
            }

            return (None, context);
        }
    };
}

fn interpret_assignment<'a,'b>(line: &'b Assignment<'a>, mut context: Box<ExecutionContext<'a>>) -> Box<ExecutionContext<'a>> {
    let result = interpret_expr(&line.right, &*context);
    context.set_value(line.left, result);
    return context;
}

fn interpret_expr<'a, 'b>(line: &Expr, context: &ExecutionContext<'a>) -> Value where 'a : 'b {
    if let Expr::Value(v) = line {
        return v.clone();
    }

    if let Expr::Identifier(id) = line {
        return context.get_value(*id).unwrap_or_else(|| panic!("Referenced uninitialized variable {:?}", *id)).clone();
    }

    if let Expr::BinaryOperation(op) = line {
        let l= interpret_expr(&*op.left, context);
        let r = interpret_expr(&*op.right, context);

        return match op.operator {
            BinaryOperator::Add => l + r,
            BinaryOperator::Subtract => l - r,
            BinaryOperator::Multiply => l * r,
            BinaryOperator::Divide => l / r,
            BinaryOperator::LogicalAnd => {
                if !l.is_truthy() {
                    return l;
                }
                return r;
            }
            BinaryOperator::LogicalOr => {
                if l.is_truthy() {
                    return l;
                }
                return r;
            },
            BinaryOperator::Equals => if l == r { Value::Integer(1) } else { Value::Integer(0) },
            BinaryOperator::NotEqual =>  if l != r { Value::Integer(1) } else { Value::Integer(0) },
        };
    };

    if let Expr::FunctionCall(call) = line {
        let definition = context.get_function(call.function_name).unwrap_or_else(|| panic!("Unknown function {}", call.function_name));
        if call.arguments.len() != definition.parameters.len() {
            panic!("Expected {} arguments for function {}, received {}", definition.parameters.len(), call.function_name, call.arguments.len())
        }

        let mut new_context = ExecutionContext::new();
        // new_context.parent_scope = Some(context); TODO: Get this working
        new_context.variables = context.variables.clone();
        new_context.functions = context.functions.clone();

        for i in 0..definition.parameters.len() {
            let result = interpret_expr(&call.arguments[i], context);
            new_context.set_value(definition.parameters[i], result);
        }

        return match &definition.body.as_ref() {
            FunctionBody::Expr(e) => interpret_expr(e, &new_context),
            FunctionBody::Intrinsic(s) => {
                return match s.as_ref() {
                    "print" => {
                        print!("{}", new_context.variables.get("$0").unwrap_or(&Value::None).format());
                        return Value::None;
                    },
                    "println" => {
                        println!("{:?}", new_context.variables.get("$0").unwrap_or(&Value::None).format());
                        return Value::None;
                    },
                    _ => panic!("Unknown intrinsic {}", s),
                }
            }
            _ => panic!("Can't handle intrinsics yet")
        }
    }

    if let Expr::Block(block) = line {
        // Essentially executes in a copy of the current context, where any writes are ignored outside of it
        // new_context.parent_scope = Some(context.parent_scope); TODO: Get this working
        let mut new_context = ExecutionContext::new();
        new_context.variables = context.variables.clone();
        new_context.functions = context.functions.clone();

        let (_, result) = interpret_ast(block, Box::new(new_context));
        return result.unwrap_or(Value::None);
    }

    unreachable!("Unknown Expr: {:?}", line);
}

fn main() {
    let stdin = io::stdin();
    let input = io::read_to_string(stdin).unwrap_or_else(|e| panic!("Could not read input: {}", e));

    if let Ok(pairs) = ShellParser::parse(Rule::lines, &input) {
        let ast = create_ast(pairs);

        let mut context = Box::new(ExecutionContext::new());
        context.functions.insert("print".to_string(), Function {
            parameters: Rc::new(vec!["$0"]),
            body: Rc::new(FunctionBody::Intrinsic("print".to_string()))
        });
        context.functions.insert("println".to_string(), Function {
            parameters: Rc::new(vec!["$0"]),
            body: Rc::new(FunctionBody::Intrinsic("println".to_string()))
        });
        let (c, result) = interpret_ast(&ast, context);
        println!("{:?}", result);
    } else {
        panic!("Invalid input");
    }
}
