use pest::{iterators::Pair, Parser};

use crate::ast;

mod air {
    // The pest_derive macro makes "Rule" public, so placing this into a nested
    // module to prevent it from being exposed.
    use pest_derive::Parser;

    #[derive(Parser)]
    #[grammar = "air.pest"]
    pub struct AirParser;
}
