#[derive(Debug)]
pub struct Source<'a> {
    pub span: Span<'a>,
    pub forms: Vec<Form<'a>>,
}

#[derive(Debug)]
pub struct Form<'a> {
    pub span: Span<'a>,
    pub data: FormData<'a>,
}

#[derive(Debug)]
pub enum FormData<'a> {
    Int(IntData),
    Str(StrData),
    Label(String),
    Sym(String),
    Ident(String),
    GlobalPath(Vec<PathSegment<'a>>),
    LocalPath(Vec<PathSegment<'a>>),
    Discard(String),
    Parens(Vec<Form<'a>>),
    Bracks(Vec<Form<'a>>),
    Braces(Vec<Form<'a>>),
    Op(String),
    Prefix(String),
    Infix(String),
    Postfix(String),
    Sep,
}

#[derive(Debug)]
pub enum IntData {
    UntypedPos(u128),
    UntypedNeg(i128),
}

#[derive(Debug)]
pub enum StrData {
    Literal(String),
}

#[derive(Debug)]
pub struct PathSegment<'a> {
    pub span: Span<'a>,
    pub value: String,
}

#[derive(Debug)]
pub struct Span<'a> {
    pub str: &'a str,
    pub range: (u32, u32),
    pub pos: (u32, u32),
}
