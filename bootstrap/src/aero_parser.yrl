%-- Nonterminals and Terminals ------------------------------------------------

Nonterminals
  source
  group
  group_bare
  primary
  secondary
  simple
  block
  macro_call
  macro_args
  whitespace
  .

Terminals
  ident
  atom_lit
  string_lit
  char_lit
  newline
  space
  '{'
  '}'
  ','
  .

%-- Global Settings -----------------------------------------------------------

Rootsymbol source.

Expect 0.

%-- Precedence ----------------------------------------------------------------

Unary 10 newline.
Left  20 space.
Left  30 ','.

%-- Source --------------------------------------------------------------------

% The entire source is just a wrapper around an expression group.

source -> group : {source, [], '$1'}.

%-- General Expressions -------------------------------------------------------

% An expression group is just a collection of primary expressions separated by
% newlines. These are used for the entire source as well as blocks. There are
% two kinds of expressions: primary and secondary. Primary expressions are
% those at the top (i.e. in used in expression groups) and secondary ones are
% those used inside of a primary expression. Primary expressions always end
% with newlines.

block -> '{' group '}' : {block, get_meta('$1'), '$2'}.

group -> group_bare : '$1'.
group -> group_bare whitespace : '$1'.
group -> whitespace group_bare : '$2'.
group -> whitespace group_bare whitespace : '$2'.

group_bare -> '$empty' : [].
group_bare -> primary : ['$1'].
group_bare -> primary newline group : ['$1' | '$3'].

primary -> block : '$1'.
primary -> macro_call : '$1'.
primary -> simple : '$1'.

secondary -> block : '$1'.
secondary -> simple : '$1'.

simple -> ident : token_to_ast('$1').
simple -> atom_lit : token_to_ast('$1').
simple -> string_lit : token_to_ast('$1').

%-- Macro Calls ---------------------------------------------------------------

macro_call -> ident space macro_args : macro_call('$1', '$3').

macro_args -> secondary : [pos_arg('$1')].
macro_args -> secondary ',' macro_args : [pos_arg('$1') | '$3'].
macro_args -> secondary ',' whitespace macro_args : [pos_arg('$1') | '$4'].
macro_args -> secondary space block : [pos_arg('$1'), pos_arg('$3')].

%-- Whitespace ----------------------------------------------------------------

% Whitespace in Aero is either a newline or a space. Note that a newline is the
% lexer is an amount of whitespace that contains at least one "\n" character.
% There cannot be two whitespace (newline or space) in a row. Spaces are needed
% in the parser so that macro calls can be differentiated from function calls.

whitespace -> newline.
whitespace -> space.

%-- Helper Functions ----------------------------------------------------------

Erlang code.

% Build an AST node for a macro call.
macro_call(Ident, Args) ->
  {macro_call, get_meta(Ident), [
    {name, token_to_ast(Ident)},
    {args, Args}
  ]}.

% Build an AST node for a positional argument.
pos_arg(Arg) ->
  {pos_arg, get_meta(Arg), Arg}.

% Convert a token containing a value to an AST node.
token_to_ast({Category, _, Value} = Token) ->
  {Category, get_meta(Token), Value}.

% Get a token's line number and put it in metadata.
get_meta({_, Line}) when is_integer(Line) ->
  [{line, Line}];

get_meta({_, Line, _}) when is_integer(Line) ->
  [{line, Line}];

get_meta({_, Meta, _}) when is_list(Meta) ->
  Meta.
