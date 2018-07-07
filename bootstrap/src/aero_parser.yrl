Nonterminals
  grammar
  block
  .

Terminals
  ident
  atom_lit
  string_lit
  char_lit
  newline
  space
  '+'
  '-'
  '**'
  '*'
  '/'
  '('
  ')'
  '['
  ']'
  '{'
  '}'
  ','
  .

Rootsymbol grammar.

Expect 0.

grammar -> '$empty' : {source, [], []}.

Erlang code.

% Get the token value from the tuple from the lexer.
extract_token({_Token, _Line, Value}) ->
  Value.
