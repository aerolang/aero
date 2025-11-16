use pest::{iterators::Pair, Parser};

use crate::ast::*;

mod air {
    // The pest_derive macro makes "Rule" public, so placing this into a nested
    // module to prevent it from being exposed.
    use pest_derive::Parser;

    #[derive(Parser)]
    #[grammar = "air.pest"]
    pub struct AirParser;
}

use air::AirParser;

#[derive(Debug)]
pub enum ParseError {
    PestError(Box<pest::error::Error<air::Rule>>),
    InvalidRule(String),
}

impl From<pest::error::Error<air::Rule>> for ParseError {
    fn from(err: pest::error::Error<air::Rule>) -> Self {
        ParseError::PestError(Box::new(err))
    }
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            ParseError::PestError(e) => write!(f, "{}", e),
            ParseError::InvalidRule(s) => write!(f, "Invalid rule: {}", s),
        }
    }
}

impl std::error::Error for ParseError {}

pub fn parse_air(source: &str) -> Result<Source, ParseError> {
    let mut pairs = AirParser::parse(air::Rule::Source, source)?;
    let source_pair = pairs.next().unwrap();
    Ok(parse_source(source, source_pair))
}

fn make_span<'a>(source: &'a str, pair: &Pair<air::Rule>) -> Span<'a> {
    let span = pair.as_span();
    let start = span.start();
    let end = span.end();
    let (line, col) = span.start_pos().line_col();

    Span {
        str: source,
        range: (start as u32, end as u32),
        pos: (line as u32, col as u32),
    }
}

fn parse_source<'a>(source: &'a str, pair: Pair<'a, air::Rule>) -> Source<'a> {
    let span = make_span(source, &pair);
    let mut defs = Vec::new();

    let mut inner_iter = pair.into_inner().peekable();

    while let Some(inner) = inner_iter.next() {
        match inner.as_rule() {
            air::Rule::Pub => {
                // Next item should be MainDef or FuncDef
                if let Some(def_pair) = inner_iter.next() {
                    if let Some(def) = parse_def(source, def_pair, Visibility::Pub) {
                        defs.push(def);
                    }
                }
            }
            air::Rule::Priv => {
                // Next item should be MainDef or FuncDef
                if let Some(def_pair) = inner_iter.next() {
                    if let Some(def) = parse_def(source, def_pair, Visibility::Priv) {
                        defs.push(def);
                    }
                }
            }
            air::Rule::EOI => {}
            _ => {}
        }
    }

    Source { span, data: defs }
}

fn parse_def<'a>(source: &'a str, pair: Pair<'a, air::Rule>, vis: Visibility) -> Option<Def<'a>> {
    let span = make_span(source, &pair);

    match pair.as_rule() {
        air::Rule::MainDef => {
            // Grammar: "main" ~ "do:" ~ (Assign)* ~ Expr
            let inner = pair.into_inner();
            let mut assigns = Vec::new();
            let mut result = None;

            for part in inner {
                match part.as_rule() {
                    air::Rule::Assign => {
                        assigns.push(parse_assign(source, part));
                    }
                    air::Rule::Void | air::Rule::Str | air::Rule::Sym |
                    air::Rule::VarName | air::Rule::DefName | air::Rule::Call => {
                        result = Some(parse_expr(source, part));
                    }
                    _ => {}
                }
            }

            Some(Def {
                span: span.clone(),
                vis,
                data: DefData::Main {
                    body: Block {
                        span,
                        assigns,
                        result: result?,
                    },
                },
            })
        }
        air::Rule::FuncDef => {
            // Grammar: "func" ~ DefName ~ ParamList ~ "->" ~ Type ~ "do:" ~ (Assign)* ~ Expr
            let inner = pair.into_inner();
            let mut name = None;
            let mut params = Vec::new();
            let mut return_type = None;
            let mut assigns = Vec::new();
            let mut result = None;

            for part in inner {
                match part.as_rule() {
                    air::Rule::DefName => {
                        name = Some(parse_defname(source, part));
                    }
                    air::Rule::ParamList => {
                        params = parse_param_list(source, part);
                    }
                    air::Rule::IntType | air::Rule::StrType | air::Rule::VoidType => {
                        return_type = Some(parse_type(source, part));
                    }
                    air::Rule::Assign => {
                        assigns.push(parse_assign(source, part));
                    }
                    air::Rule::Void | air::Rule::Str | air::Rule::Sym |
                    air::Rule::VarName | air::Rule::Call => {
                        result = Some(parse_expr(source, part));
                    }
                    _ => {}
                }
            }

            Some(Def {
                span: span.clone(),
                vis,
                data: DefData::Func {
                    name: name?,
                    params,
                    return_type: return_type?,
                    body: Block {
                        span,
                        assigns,
                        result: result?,
                    },
                },
            })
        }
        _ => None,
    }
}

fn parse_assign<'a>(source: &'a str, pair: Pair<'a, air::Rule>) -> Assign<'a> {
    let span = make_span(source, &pair);
    let mut inner = pair.into_inner();

    let var = parse_varname(source, inner.next().unwrap());
    let ty = parse_type(source, inner.next().unwrap());
    let expr = parse_expr(source, inner.next().unwrap());

    Assign { span, var, ty, expr }
}

fn parse_expr<'a>(source: &'a str, pair: Pair<'a, air::Rule>) -> Expr<'a> {
    let span = make_span(source, &pair);

    let data = match pair.as_rule() {
        air::Rule::Call => {
            ExprData::Call(parse_call(source, pair))
        }
        _ => {
            ExprData::Simple(parse_simple_data(source, pair))
        }
    };

    Expr { span, data }
}

fn parse_call<'a>(source: &'a str, pair: Pair<'a, air::Rule>) -> CallData<'a> {
    // Call = { "(" ~ Callee ~ (SimpleExpr)* ~ ")" }
    let mut inner = pair.into_inner();
    let callee_pair = inner.next().unwrap();

    let callee_span = make_span(source, &callee_pair);
    let callee_data = match callee_pair.as_rule() {
        air::Rule::Builtin => {
            let inner_name = callee_pair.into_inner().next().unwrap();
            CalleeData::Builtin(inner_name.as_str())
        }
        air::Rule::DefName => {
            let inner_name = callee_pair.into_inner().next().unwrap();
            CalleeData::DefName(inner_name.as_str())
        }
        air::Rule::VarName => {
            let inner_name = callee_pair.into_inner().next().unwrap();
            CalleeData::VarName(inner_name.as_str())
        }
        rule => panic!("Unexpected callee type: {:?}", rule),
    };

    let args: Vec<Simple> = inner.map(|arg| parse_simple(source, arg)).collect();

    CallData {
        callee: Callee {
            span: callee_span,
            data: callee_data,
        },
        args,
    }
}

fn parse_param_list<'a>(source: &'a str, pair: Pair<'a, air::Rule>) -> Vec<Param<'a>> {
    pair.into_inner()
        .filter(|p| p.as_rule() == air::Rule::Param)
        .map(|param_pair| parse_param(source, param_pair))
        .collect()
}

fn parse_param<'a>(source: &'a str, pair: Pair<'a, air::Rule>) -> Param<'a> {
    let span = make_span(source, &pair);
    let mut inner = pair.into_inner();

    let name = parse_varname(source, inner.next().unwrap());
    let ty = parse_type(source, inner.next().unwrap());

    Param { span, name, ty }
}

fn parse_simple<'a>(source: &'a str, pair: Pair<'a, air::Rule>) -> Simple<'a> {
    let span = make_span(source, &pair);
    let data = parse_simple_data(source, pair);
    Simple { span, data }
}

fn parse_simple_data<'a>(_source: &'a str, pair: Pair<'a, air::Rule>) -> SimpleData<'a> {
    match pair.as_rule() {
        air::Rule::Void => SimpleData::Void,
        air::Rule::Str => {
            let inner = pair.into_inner().next().unwrap(); // StrContent
            SimpleData::Str(inner.as_str())
        }
        air::Rule::Sym => {
            let inner = pair.into_inner().next().unwrap(); // Name
            SimpleData::Sym(inner.as_str())
        }
        air::Rule::VarName => {
            let inner = pair.into_inner().next().unwrap(); // Name
            SimpleData::VarName(inner.as_str())
        }
        air::Rule::DefName => {
            let inner = pair.into_inner().next().unwrap(); // Name
            SimpleData::DefName(inner.as_str())
        }
        air::Rule::Name => {
            SimpleData::Name(pair.as_str())
        }
        rule => panic!("Unexpected simple data type: {:?}", rule),
    }
}

fn parse_type<'a>(source: &'a str, pair: Pair<'a, air::Rule>) -> Type<'a> {
    let span = make_span(source, &pair);
    let data = match pair.as_rule() {
        air::Rule::IntType => TypeData::Int,
        air::Rule::StrType => TypeData::Str,
        air::Rule::VoidType => TypeData::Void,
        rule => panic!("Unexpected type: {:?}", rule),
    };

    Type { span, data }
}

fn parse_varname<'a>(source: &'a str, pair: Pair<'a, air::Rule>) -> VarName<'a> {
    let span = make_span(source, &pair);
    let inner = pair.into_inner().next().unwrap(); // Name
    VarName {
        span,
        value: inner.as_str(),
    }
}

fn parse_defname<'a>(source: &'a str, pair: Pair<'a, air::Rule>) -> DefName<'a> {
    let span = make_span(source, &pair);
    let inner = pair.into_inner().next().unwrap(); // Name
    DefName {
        span,
        value: inner.as_str(),
    }
}
