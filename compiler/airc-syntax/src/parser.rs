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
                // Next item should be FuncDef
                if let Some(func_def) = inner_iter.next() {
                    if let Some(def) = parse_def(source, func_def, Visibility::Pub) {
                        defs.push(def);
                    }
                }
            }
            air::Rule::Priv => {
                // Next item should be FuncDef
                if let Some(func_def) = inner_iter.next() {
                    if let Some(def) = parse_def(source, func_def, Visibility::Priv) {
                        defs.push(def);
                    }
                }
            }
            air::Rule::FuncDef => {
                // FuncDef without explicit visibility (default to Priv)
                if let Some(def) = parse_def(source, inner, Visibility::Priv) {
                    defs.push(def);
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

    // FuncDef is the only DefInner currently
    // Grammar: "func" ~ DefName ~ "->" ~ Type ~ "do:" ~ (Assign)* ~ Expr

    let inner = pair.into_inner();
    let mut name = None;
    let mut return_type = None;
    let mut assigns = Vec::new();
    let mut result = None;

    for part in inner {
        match part.as_rule() {
            air::Rule::DefName => {
                name = Some(parse_defname(source, part));
            }
            air::Rule::IntType | air::Rule::StrType | air::Rule::VoidType => {
                return_type = Some(parse_type(source, part));
            }
            air::Rule::Assign => {
                assigns.push(parse_assign(source, part));
            }
            air::Rule::Void | air::Rule::Str | air::Rule::Sym |
            air::Rule::VarName | air::Rule::DefName | air::Rule::LogCall => {
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
            return_type: return_type?,
            body: Block {
                span,
                assigns,
                result: result?,
            },
        },
    })
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
        air::Rule::LogCall => {
            ExprData::Call(parse_call(source, pair))
        }
        _ => {
            ExprData::Simple(parse_simple_data(source, pair))
        }
    };

    Expr { span, data }
}

fn parse_call<'a>(source: &'a str, pair: Pair<'a, air::Rule>) -> CallData<'a> {
    let span = make_span(source, &pair);

    // LogCall = { "log" ~ SimpleExpr }
    let mut inner = pair.into_inner();
    let arg = parse_simple(source, inner.next().unwrap());

    CallData {
        callee: Callee {
            span,
            data: CalleeData::Name("log"),
        },
        args: vec![arg],
    }
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
        _ => SimpleData::Void, // Fallback
    }
}

fn parse_type<'a>(source: &'a str, pair: Pair<'a, air::Rule>) -> Type<'a> {
    let span = make_span(source, &pair);
    let data = match pair.as_rule() {
        air::Rule::IntType => TypeData::Int,
        air::Rule::StrType => TypeData::Str,
        air::Rule::VoidType => TypeData::Void,
        _ => TypeData::Void, // Fallback
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
