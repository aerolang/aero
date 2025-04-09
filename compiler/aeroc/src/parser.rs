use std::process::exit;

use pest::{iterators::Pair, Parser};

use crate::ast::{Form, FormData, IntData, PathSegment, Source, Span, StrData};

mod aero {
    // The pest_derive macro makes "Rule" public, so placing this into a nested
    // module to prevent it from being exposed.
    use pest_derive::Parser;

    #[derive(Parser)]
    #[grammar = "aero.pest"]
    pub struct AeroParser;
}

use aero::{AeroParser, Rule};

pub fn parse<'a>(input: &'a str) -> Source {
    let mut pairs = AeroParser::parse(aero::Rule::Source, input).unwrap_or_else(|e| {
        eprintln!("{}", e);
        exit(1);
    });

    let source_pair = pairs.next().unwrap();

    let span = get_span(&source_pair);
    let forms = parse_forms(
        source_pair
            .into_inner()
            .take_while(|p| p.as_rule() != Rule::EOI),
    );

    Source { span, forms }
}

fn parse_forms<'a, T: Iterator<Item = Pair<'a, Rule>>>(pairs: T) -> Vec<Form<'a>> {
    pairs.map(parse_form).collect()
}

fn parse_form(pair: Pair<Rule>) -> Form {
    let span = get_span(&pair);
    let data = match pair.as_rule() {
        Rule::Int => FormData::Int(parse_int_data(pair.into_inner())),
        Rule::Str => FormData::Str(parse_str_data(pair.into_inner())),
        Rule::Label => FormData::Label(parse_name(pair.into_inner())),
        Rule::Sym => FormData::Sym(parse_name(pair.into_inner())),
        Rule::Ident => FormData::Ident(parse_name(pair.into_inner())),
        Rule::GlobalPath => FormData::GlobalPath(parse_path_segments(pair.into_inner())),
        Rule::LocalPath => FormData::LocalPath(parse_path_segments(pair.into_inner())),
        Rule::Discard => FormData::Discard(String::from(span.str)),
        Rule::Parens => FormData::Parens(parse_forms(pair.into_inner())),
        Rule::Bracks => FormData::Bracks(parse_forms(pair.into_inner())),
        Rule::Braces => FormData::Braces(parse_forms(pair.into_inner())),
        Rule::Op => FormData::Op(String::from(span.str)),
        Rule::Prefix => FormData::Prefix(String::from(span.str)),
        Rule::Infix => FormData::Infix(String::from(span.str)),
        Rule::Postfix => FormData::Postfix(String::from(span.str)),
        Rule::Sep => FormData::Sep,
        _ => unreachable!(),
    };

    Form { span, data }
}

fn parse_int_data<'a, T: Iterator<Item = Pair<'a, Rule>>>(mut pairs: T) -> IntData {
    let mut pair = pairs.next().unwrap();

    let is_positive = match pair.as_rule() {
        Rule::IntSignPos => {
            pair = pairs.next().unwrap();
            true
        }
        Rule::IntSignNeg => {
            pair = pairs.next().unwrap();
            false
        }
        _ => true,
    };

    let value = pair.as_str().replace("_", "").parse().unwrap();

    if is_positive {
        IntData::UntypedPos(value)
    } else {
        IntData::UntypedNeg(value as i128)
    }
}

fn parse_str_data<'a, T: Iterator<Item = Pair<'a, Rule>>>(mut pairs: T) -> StrData {
    let literal = String::from(pairs.next().unwrap().as_str());

    StrData::Literal(literal)
}

fn parse_name<'a, T: Iterator<Item = Pair<'a, Rule>>>(mut pairs: T) -> String {
    String::from(pairs.next().unwrap().as_str())
}

fn parse_path_segments<'a, T: Iterator<Item = Pair<'a, Rule>>>(pairs: T) -> Vec<PathSegment<'a>> {
    pairs.map(parse_path_segment).collect()
}

fn parse_path_segment(pair: Pair<Rule>) -> PathSegment {
    let span = get_span(&pair);
    let value = String::from(span.str);

    PathSegment { span, value }
}

fn get_span<'a>(pair: &Pair<'a, Rule>) -> Span<'a> {
    let pair_span = pair.as_span();
    let pair_line_col = pair_span.start_pos().line_col();

    Span {
        str: pair.as_str(),
        range: (pair_span.start() as u32, pair_span.end() as u32),
        pos: (pair_line_col.0 as u32, pair_line_col.1 as u32),
    }
}
