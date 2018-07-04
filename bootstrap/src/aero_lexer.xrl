Definitions.

COMMENT    = --[^\n]*
IDENT      = [a-zA-Z_][a-zA-Z0-9_]*
STRING     = "[^\"]*"
CHAR       = '[^\']'
WHITESPACE = [\s\t\r\n;]+

Rules.

{COMMENT}    : skip_token.
{IDENT}      : {token, {ident, TokenLine, list_to_atom(TokenChars)}}.
\:{IDENT}    : {token, {atom_lit, TokenLine, parse_atom(TokenChars)}}.
\:{STRING}   : {token, {atom_lit, TokenLine, parse_atom_quoted(TokenChars)}}.
{STRING}     : {token, {string_lit, TokenLine, parse_string(TokenChars)}}.
{CHAR}       : {token, {char_lit, TokenLine, parse_char(TokenChars)}}.
{WHITESPACE} : {token, {whitespace_type(TokenChars), TokenLine}}.
{NEWLINE}    : {token, {newline, TokenLine}}.
{SPACE}      : {token, {space, TokenLine}}.
\+           : {token, {'+', TokenLine}}.
-            : {token, {'+', TokenLine}}.
\*\*         : {token, {'**', TokenLine}}.
\*           : {token, {'*', TokenLine}}.
/            : {token, {'/', TokenLine}}.
\(           : {token, {'(', TokenLine}}.
\)           : {token, {')', TokenLine}}.
\[           : {token, {'[', TokenLine}}.
\]           : {token, {']', TokenLine}}.
\{           : {token, {'{', TokenLine}}.
\}           : {token, {'}', TokenLine}}.
,            : {token, {',', TokenLine}}.

Erlang code.

% Convert chars starting with a colon to an atom.
parse_atom([$: | Chars]) ->
  list_to_atom(Chars).

% Convert atom literal with name in quotes for special characters.
parse_atom_quoted([$: | Chars]) ->
  list_to_atom(lists:sublist(Chars, 2, length(Chars) - 2)).

% Convert a charlist to a binary.
parse_string(Chars) ->
  list_to_binary(lists:sublist(Chars, 2, length(Chars) - 2)).

% Extract the char value from a charlist.
parse_char(Chars) ->
  lists:nth(2, Chars).

% Classify whitespace as being a normal space or newline.
whitespace_type(Chars) ->
  case re:run(Chars, "[\n;]") of
    nomatch -> space;
    _       -> newline
  end.
