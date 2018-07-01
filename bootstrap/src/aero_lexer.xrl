Definitions.

COMMENT    = --[^\n]*
SNAKE_CASE = [a-z_][a-z0-9_]*
TITLE_CASE = [A-Z][a-zA-Z0-9]*
SPECIAL    = __[A-Z][A-Z0-9_]*[A-Z0-9]__
STRING     = "[^\"]*"
CHAR       = '[^\']'
WHITESPACE = [\s\t\r\n]+

Rules.

{COMMENT}      : skip_token.
{SNAKE_CASE}   : {token, {snake_case, TokenLine, list_to_atom(TokenChars)}}.
{TITLE_CASE}   : {token, {title_case, TokenLine, list_to_atom(TokenChars)}}.
{SPECIAL}      : {token, {special, TokenLine, list_to_atom(TokenChars)}}.
\:{SNAKE_CASE} : {token, {atom_lit, TokenLine, parse_atom(TokenChars)}}.
\:{STRING}     : {token, {atom_lit, TokenLine, parse_atom_quoted(TokenChars)}}.
{STRING}       : {token, {string_lit, TokenLine, parse_string(TokenChars)}}.
{CHAR}         : {token, {char_lit, TokenLine, parse_char(TokenChars)}}.
{WHITESPACE}   : {token, {whitespace_type(TokenChars), TokenLine}}.
{NEWLINE}      : {token, {newline, TokenLine}}.
{SPACE}        : {token, {space, TokenLine}}.
\+             : {token, {'+', TokenLine}}.
-              : {token, {'+', TokenLine}}.
\*\*           : {token, {'**', TokenLine}}.
\*             : {token, {'*', TokenLine}}.
/              : {token, {'/', TokenLine}}.
\(             : {token, {'(', TokenLine}}.
\)             : {token, {')', TokenLine}}.
\[             : {token, {'[', TokenLine}}.
\]             : {token, {']', TokenLine}}.
\{             : {token, {'{', TokenLine}}.
\}             : {token, {'}', TokenLine}}.
,              : {token, {',', TokenLine}}.

Erlang code.

% Convert chars starting with a colon to an atom.
parse_atom([$: | Chars]) ->
  list_to_atom(Chars).

% Convert atom literal with name in quotes for special characters.
parse_atom_quoted([$: | Chars]) ->
  list_to_atom(string:slice(Chars, 1, string:length(Chars) - 2)).

% Convert a charlist to a binary.
parse_string(Chars) ->
  list_to_binary(string:slice(Chars, 1, string:length(Chars) - 2)).

% Extract the char value from a charlist.
parse_char(Chars) ->
  lists:nth(2, Chars).

% Classify whitespace as being a normal space or newline.
whitespace_type(Chars) ->
  case string:find(Chars, "\n") of
    nomatch -> space;
    _       -> newline
  end.
