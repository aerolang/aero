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
  .

Terminals
  ident
  atom_lit
  string_lit
  newline
  '{'
  '}'
  ','
  .

%-- Global Settings -----------------------------------------------------------

Rootsymbol source.

Expect 0.

%-- Precedence ----------------------------------------------------------------

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

group -> '$empty' : [].
group -> newline : [].
group -> group_bare : reverse('$1').
group -> group_bare newline : reverse('$1').
group -> newline group_bare : reverse('$2').
group -> newline group_bare newline : reverse('$2').

% Note that this is in reverse order (fixed above).
group_bare -> primary : ['$1'].
group_bare -> group_bare newline primary : ['$3' | '$1'].

primary -> block : '$1'.
primary -> macro_call : '$1'.
primary -> simple : '$1'.

secondary -> block : '$1'.
secondary -> simple : '$1'.

simple -> ident : token_to_ast('$1').
simple -> atom_lit : token_to_ast('$1').
simple -> string_lit : token_to_ast('$1').

%-- Macro Calls ---------------------------------------------------------------

macro_call -> ident macro_args : macro_call('$1', '$2').

macro_args -> secondary : [pos_arg('$1')].
macro_args -> secondary ',' macro_args : [pos_arg('$1') | '$3'].
macro_args -> secondary ',' newline macro_args : [pos_arg('$1') | '$4'].
macro_args -> secondary block : [pos_arg('$1'), pos_arg('$2')].

%-- Helper Functions ----------------------------------------------------------

Erlang code.

-import(lists, [reverse/1]).

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
