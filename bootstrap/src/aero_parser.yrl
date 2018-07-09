Nonterminals
  source
  expr_group
  expr
  macro_call
  macro_args
  opt_whitespace
  whitespace
  .

Terminals
  ident
  atom_lit
  string_lit
  char_lit
  newline
  space
  .

Rootsymbol source.

Expect 0.

Unary 10 space.
Left  20 newline.

source -> opt_whitespace expr_group opt_whitespace: {source, [], '$2'}.

opt_whitespace -> '$empty'.
opt_whitespace -> whitespace.

whitespace -> newline.
whitespace -> space.

expr_group -> '$empty' : [].
expr_group -> expr : ['$1'].
expr_group -> expr newline expr_group : ['$1' | '$3'].

expr -> macro_call : '$1'.
expr -> ident : token_to_ast('$1').
expr -> atom_lit : token_to_ast('$1').
expr -> string_lit : token_to_ast('$1').
expr -> char_lit : token_to_ast('$1').

macro_call -> ident space macro_args : build_macro_call('$1', '$3').

macro_args -> expr : [{pos_arg, get_meta('$1'), '$1'}].

Erlang code.

% Build an AST node for a macro call.
build_macro_call(Ident, Args) ->
  {macro_call, get_meta(Ident), [
    {name, token_to_ast(Ident)},
    {args, Args}
  ]}.

% Convert a token containing a value to an AST node.
token_to_ast({Category, _, Value} = Token) ->
  {Category, get_meta(Token), Value}.

% Get a token's line number and put it in metadata.
get_meta({_, Line, _}) when is_integer(Line) ->
  [{line, Line}];

get_meta({_, Meta, _}) when is_list(Meta) ->
  Meta.
