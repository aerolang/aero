use pest::{Parser, iterators::Pair};

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

impl ParseError {
    /// Returns (line, col), both 1-based. Returns (1, 1) when no location is available.
    pub fn line_col(&self) -> (usize, usize) {
        match self {
            ParseError::PestError(e) => match e.line_col {
                pest::error::LineColLocation::Pos((l, c))
                | pest::error::LineColLocation::Span((l, c), _) => (l, c),
            },
            ParseError::InvalidRule(_) => (1, 1),
        }
    }

    /// Returns the error message text only (no location context).
    pub fn message(&self) -> String {
        match self {
            ParseError::PestError(e) => match &e.variant {
                pest::error::ErrorVariant::ParsingError {
                    positives,
                    negatives,
                } => {
                    let mut parts = Vec::new();
                    if !positives.is_empty() {
                        parts.push(format!(
                            "expected {}",
                            positives
                                .iter()
                                .map(|r| format!("{r:?}"))
                                .collect::<Vec<_>>()
                                .join(", ")
                        ));
                    }
                    if !negatives.is_empty() {
                        parts.push(format!(
                            "unexpected {}",
                            negatives
                                .iter()
                                .map(|r| format!("{r:?}"))
                                .collect::<Vec<_>>()
                                .join(", ")
                        ));
                    }
                    if parts.is_empty() {
                        "parse error".to_string()
                    } else {
                        parts.join("; ")
                    }
                }
                pest::error::ErrorVariant::CustomError { message } => message.clone(),
            },
            ParseError::InvalidRule(s) => format!("invalid rule: {s}"),
        }
    }
}

pub fn parse_air(source: &str) -> Result<Source<'_>, ParseError> {
    let mut pairs = AirParser::parse(air::Rule::Source, source)?;
    let source_pair = pairs.next().unwrap();
    parse_source(source, source_pair)
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

fn parse_source<'a>(source: &'a str, pair: Pair<'a, air::Rule>) -> Result<Source<'a>, ParseError> {
    let span = make_span(source, &pair);
    let mut defs = Vec::new();

    let mut inner_iter = pair.into_inner().peekable();

    while let Some(inner) = inner_iter.next() {
        match inner.as_rule() {
            air::Rule::Pub => {
                if let Some(def_pair) = inner_iter.next()
                    && let Some(def) = parse_def(source, def_pair, Visibility::Pub)?
                {
                    defs.push(def);
                }
            }
            air::Rule::Priv => {
                if let Some(def_pair) = inner_iter.next()
                    && let Some(def) = parse_def(source, def_pair, Visibility::Priv)?
                {
                    defs.push(def);
                }
            }
            air::Rule::EOI => {}
            _ => {}
        }
    }

    Ok(Source { span, data: defs })
}

fn parse_def<'a>(
    source: &'a str,
    pair: Pair<'a, air::Rule>,
    vis: Visibility,
) -> Result<Option<Def<'a>>, ParseError> {
    let span = make_span(source, &pair);

    match pair.as_rule() {
        air::Rule::MainDef => {
            let inner = pair.into_inner();
            let mut assigns = Vec::new();
            let mut result = None;

            for part in inner {
                match part.as_rule() {
                    air::Rule::Assign => {
                        assigns.push(parse_assign(source, part)?);
                    }
                    air::Rule::Void
                    | air::Rule::Int
                    | air::Rule::Str
                    | air::Rule::FieldAccess
                    | air::Rule::VarName
                    | air::Rule::DefName
                    | air::Rule::Call
                    | air::Rule::IfExpr
                    | air::Rule::Construct => {
                        result = Some(parse_expr(source, part)?);
                    }
                    _ => {}
                }
            }

            Ok(Some(Def {
                span: span.clone(),
                vis,
                data: DefData::Main {
                    body: Block {
                        span,
                        assigns,
                        result: result.ok_or_else(|| {
                            ParseError::InvalidRule("main body missing result expression".into())
                        })?,
                    },
                },
            }))
        }
        air::Rule::FuncDef => {
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
                        params = parse_param_list(source, part)?;
                    }
                    air::Rule::IntType
                    | air::Rule::StrType
                    | air::Rule::BoolType
                    | air::Rule::VoidType
                    | air::Rule::StructType => {
                        return_type = Some(parse_type(source, part)?);
                    }
                    air::Rule::Assign => {
                        assigns.push(parse_assign(source, part)?);
                    }
                    air::Rule::Void
                    | air::Rule::Int
                    | air::Rule::Str
                    | air::Rule::FieldAccess
                    | air::Rule::VarName
                    | air::Rule::Call
                    | air::Rule::IfExpr
                    | air::Rule::Construct => {
                        result = Some(parse_expr(source, part)?);
                    }
                    _ => {}
                }
            }

            Ok(Some(Def {
                span: span.clone(),
                vis,
                data: DefData::Func {
                    name: name
                        .ok_or_else(|| ParseError::InvalidRule("func missing name".into()))?,
                    params,
                    return_type: return_type.ok_or_else(|| {
                        ParseError::InvalidRule("func missing return type".into())
                    })?,
                    body: Block {
                        span,
                        assigns,
                        result: result.ok_or_else(|| {
                            ParseError::InvalidRule("func body missing result expression".into())
                        })?,
                    },
                },
            }))
        }
        air::Rule::StructDef => {
            let mut inner = pair.into_inner();
            let name_pair = inner
                .next()
                .ok_or_else(|| ParseError::InvalidRule("struct missing name".into()))?;
            // HashName rule: "#" ~ Name; grab the inner Name
            let name = name_pair.into_inner().next().unwrap().as_str();
            let mut field_types = Vec::new();
            for part in inner {
                field_types.push(parse_type(source, part)?);
            }
            Ok(Some(Def {
                span,
                vis,
                data: DefData::Struct { name, field_types },
            }))
        }
        _ => Ok(None),
    }
}

fn parse_assign<'a>(source: &'a str, pair: Pair<'a, air::Rule>) -> Result<Assign<'a>, ParseError> {
    let span = make_span(source, &pair);
    let mut inner = pair.into_inner();

    let lhs = inner.next().unwrap();
    let target = match lhs.as_rule() {
        air::Rule::Discard => AssignTarget::Discard,
        air::Rule::VarName => AssignTarget::Var(parse_varname(source, lhs)),
        rule => {
            return Err(ParseError::InvalidRule(format!(
                "unexpected assign lhs: {:?}",
                rule
            )));
        }
    };
    let expr = parse_expr(source, inner.next().unwrap())?;

    Ok(Assign { span, target, expr })
}

fn parse_expr<'a>(source: &'a str, pair: Pair<'a, air::Rule>) -> Result<Expr<'a>, ParseError> {
    let span = make_span(source, &pair);

    let data = match pair.as_rule() {
        air::Rule::Call => ExprData::Call(parse_call(source, pair)?),
        air::Rule::IfExpr => ExprData::If(parse_if_expr(source, pair)?),
        air::Rule::Construct => {
            let mut inner = pair.into_inner().peekable();
            // Optional leading Name = the struct name (e.g. `#point`)
            let name = if inner.peek().map(|p| p.as_rule()) == Some(air::Rule::Name) {
                Some(inner.next().unwrap().as_str())
            } else {
                None
            };
            let fields: Result<Vec<Simple>, ParseError> =
                inner.map(|p| parse_simple(source, p)).collect();
            ExprData::Construct {
                name,
                fields: fields?,
            }
        }
        _ => ExprData::Simple(parse_simple_data(source, pair)?),
    };

    Ok(Expr { span, data })
}

fn parse_if_block<'a>(source: &'a str, pair: Pair<'a, air::Rule>) -> Result<Block<'a>, ParseError> {
    let span = make_span(source, &pair);
    let mut assigns = Vec::new();
    let mut result = None;

    for part in pair.into_inner() {
        match part.as_rule() {
            air::Rule::Assign => assigns.push(parse_assign(source, part)?),
            _ => result = Some(parse_expr(source, part)?),
        }
    }

    Ok(Block {
        span,
        assigns,
        result: result
            .ok_or_else(|| ParseError::InvalidRule("if block missing result expression".into()))?,
    })
}

fn parse_if_expr<'a>(source: &'a str, pair: Pair<'a, air::Rule>) -> Result<IfData<'a>, ParseError> {
    let mut inner = pair.into_inner();

    let cond_pair = inner
        .next()
        .ok_or_else(|| ParseError::InvalidRule("if: missing condition".into()))?;
    let cond = parse_simple(source, cond_pair)?;

    let then_pair = inner
        .next()
        .ok_or_else(|| ParseError::InvalidRule("if: missing then block".into()))?;
    let then_block = parse_if_block(source, then_pair)?;

    let else_pair = inner
        .next()
        .ok_or_else(|| ParseError::InvalidRule("if: missing else block".into()))?;
    let else_block = parse_if_block(source, else_pair)?;

    Ok(IfData {
        cond,
        then_block: Box::new(then_block),
        else_block: Box::new(else_block),
    })
}

fn parse_call<'a>(source: &'a str, pair: Pair<'a, air::Rule>) -> Result<CallData<'a>, ParseError> {
    let mut inner = pair.into_inner();
    let callee_pair = inner.next().unwrap();

    let callee_span = make_span(source, &callee_pair);
    let callee_data = match callee_pair.as_rule() {
        air::Rule::Builtin => {
            // Builtin = { ":" ~ Name ~ ("/" ~ Name)* }
            // as_str() gives the full ":int/add"; strip the leading ":"
            let full = callee_pair.as_str();
            CalleeData::Builtin(&full[1..])
        }
        air::Rule::DefName => {
            let inner_name = callee_pair.into_inner().next().unwrap();
            CalleeData::DefName(inner_name.as_str())
        }
        air::Rule::VarName => {
            let inner_name = callee_pair.into_inner().next().unwrap();
            CalleeData::VarName(inner_name.as_str())
        }
        rule => {
            return Err(ParseError::InvalidRule(format!(
                "unexpected callee type: {:?}",
                rule
            )));
        }
    };

    let args: Result<Vec<Simple>, ParseError> =
        inner.map(|arg| parse_simple(source, arg)).collect();

    Ok(CallData {
        callee: Callee {
            span: callee_span,
            data: callee_data,
        },
        args: args?,
    })
}

fn parse_param_list<'a>(
    source: &'a str,
    pair: Pair<'a, air::Rule>,
) -> Result<Vec<Param<'a>>, ParseError> {
    pair.into_inner()
        .filter(|p| p.as_rule() == air::Rule::Param)
        .map(|param_pair| parse_param(source, param_pair))
        .collect()
}

fn parse_param<'a>(source: &'a str, pair: Pair<'a, air::Rule>) -> Result<Param<'a>, ParseError> {
    let span = make_span(source, &pair);
    let mut inner = pair.into_inner();

    let name = parse_varname(source, inner.next().unwrap());
    let ty = parse_type(source, inner.next().unwrap())?;

    Ok(Param { span, name, ty })
}

fn parse_simple<'a>(source: &'a str, pair: Pair<'a, air::Rule>) -> Result<Simple<'a>, ParseError> {
    let span = make_span(source, &pair);
    let data = parse_simple_data(source, pair)?;
    Ok(Simple { span, data })
}

fn parse_simple_data<'a>(
    _source: &'a str,
    pair: Pair<'a, air::Rule>,
) -> Result<SimpleData<'a>, ParseError> {
    match pair.as_rule() {
        air::Rule::Void => Ok(SimpleData::Void),
        air::Rule::Bool => Ok(SimpleData::Bool(pair.as_str() == "true")),
        air::Rule::Int => {
            let n: i64 = pair.as_str().parse().expect("invalid integer literal");
            Ok(SimpleData::Int(n))
        }
        air::Rule::Str => {
            let inner = pair.into_inner().next().unwrap();
            Ok(SimpleData::Str(inner.as_str()))
        }
        air::Rule::VarName => {
            let inner = pair.into_inner().next().unwrap();
            Ok(SimpleData::VarName(inner.as_str()))
        }
        air::Rule::DefName => {
            let inner = pair.into_inner().next().unwrap();
            Ok(SimpleData::DefName(inner.as_str()))
        }
        air::Rule::FieldAccess => {
            let s = pair.as_str(); // "$name.N"
            let without_dollar = &s[1..]; // "name.N"
            let dot = without_dollar.find('.').unwrap();
            let var = &without_dollar[..dot];
            let index: u32 = without_dollar[dot + 1..]
                .parse()
                .expect("invalid field index");
            Ok(SimpleData::FieldAccess { var, index })
        }
        rule => Err(ParseError::InvalidRule(format!(
            "unexpected simple value: {:?}",
            rule
        ))),
    }
}

fn parse_type<'a>(source: &'a str, pair: Pair<'a, air::Rule>) -> Result<Type<'a>, ParseError> {
    let span = make_span(source, &pair);
    let data = match pair.as_rule() {
        air::Rule::IntType => TypeData::Int,
        air::Rule::StrType => TypeData::Str,
        air::Rule::BoolType => TypeData::Bool,
        air::Rule::VoidType => TypeData::Void,
        air::Rule::StructType => {
            // StructType = { "#" ~ Name }; inner is the Name
            let inner = pair.into_inner().next().unwrap();
            TypeData::Struct(inner.as_str().to_string())
        }
        rule => {
            return Err(ParseError::InvalidRule(format!(
                "unexpected type: {:?}",
                rule
            )));
        }
    };

    Ok(Type { span, data })
}

fn parse_varname<'a>(source: &'a str, pair: Pair<'a, air::Rule>) -> VarName<'a> {
    let span = make_span(source, &pair);
    let inner = pair.into_inner().next().unwrap();
    VarName {
        span,
        value: inner.as_str(),
    }
}

fn parse_defname<'a>(source: &'a str, pair: Pair<'a, air::Rule>) -> DefName<'a> {
    let span = make_span(source, &pair);
    let inner = pair.into_inner().next().unwrap();
    DefName {
        span,
        value: inner.as_str(),
    }
}
