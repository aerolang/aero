pub struct Source<'a> {
    pub span: Span<'a>,
    pub data: Vec<Def<'a>>,
}

pub struct Def<'a> {
    pub span: Span<'a>,
    pub vis: Visibility,
    pub data: DefData<'a>,
}

pub enum Visibility {
    Pub,
    Priv,
}

pub enum DefData<'a> {
    Main {
        body: Block<'a>,
    },
    Func {
        name: DefName<'a>,
        params: Vec<Param<'a>>,
        return_type: Type<'a>,
        body: Block<'a>,
    },
    Struct {
        name: &'a str,
        field_types: Vec<Type<'a>>,
    },
}

pub struct Param<'a> {
    pub span: Span<'a>,
    pub name: VarName<'a>,
    pub ty: Type<'a>,
}

pub struct Block<'a> {
    pub span: Span<'a>,
    pub assigns: Vec<Assign<'a>>,
    pub result: Expr<'a>,
}

pub enum AssignTarget<'a> {
    Discard,
    Var(VarName<'a>),
}

pub struct Assign<'a> {
    pub span: Span<'a>,
    pub target: AssignTarget<'a>,
    pub expr: Expr<'a>,
}

pub struct Expr<'a> {
    pub span: Span<'a>,
    pub data: ExprData<'a>,
}

pub enum ExprData<'a> {
    Call(CallData<'a>),
    Simple(SimpleData<'a>),
    If(IfData<'a>),
    Construct {
        name: Option<&'a str>,
        fields: Vec<Simple<'a>>,
    },
}

pub struct IfData<'a> {
    pub cond: Simple<'a>,
    pub then_block: Box<Block<'a>>,
    pub else_block: Box<Block<'a>>,
}

pub struct CallData<'a> {
    pub callee: Callee<'a>,
    pub args: Vec<Simple<'a>>,
}

pub struct Callee<'a> {
    pub span: Span<'a>,
    pub data: CalleeData<'a>,
}

pub enum CalleeData<'a> {
    Builtin(&'a str),
    VarName(&'a str),
    DefName(&'a str),
}

pub struct Simple<'a> {
    pub span: Span<'a>,
    pub data: SimpleData<'a>,
}

pub enum SimpleData<'a> {
    Void,
    Bool(bool),
    Int(i64),
    Str(&'a str),
    VarName(&'a str),
    DefName(&'a str),
    FieldAccess { var: &'a str, index: u32 },
}

pub struct VarName<'a> {
    pub span: Span<'a>,
    pub value: &'a str,
}

pub struct DefName<'a> {
    pub span: Span<'a>,
    pub value: &'a str,
}

pub struct Type<'a> {
    pub span: Span<'a>,
    pub data: TypeData,
}

pub enum TypeData {
    Int,
    Str,
    Bool,
    Void,
    Struct(String),
}

#[derive(Clone)]
pub struct Span<'a> {
    pub str: &'a str,
    pub range: (u32, u32),
    pub pos: (u32, u32),
}
