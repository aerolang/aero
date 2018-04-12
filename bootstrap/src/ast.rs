//! Contains the elements that form the abstract syntax tree (AST).

#[derive(PartialEq, Clone, Debug)]
pub enum Expr {
  Int32LitExpr(i32),
  BinaryExpr(BinaryOp, Box<Expr>, Box<Expr>),
  MacroCallExpr(String, Vec<Expr>),
  BlockExpr(Vec<Expr>),
  FileExpr(Vec<Expr>)
}

#[derive(PartialEq, Clone, Debug)]
pub enum BinaryOp {
  Add,
  Sub,
  Mul,
  Div
}
