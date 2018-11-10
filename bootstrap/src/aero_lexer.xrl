%-- Regex Definitions for Rules -----------------------------------------------

Definitions.

IDENT      = [a-zA-Z_][a-zA-Z0-9_]*
STRING     = "[^\"]*"
WHITESPACE = ([\s\t\r\n;]|(--[^\n]*))+

%-- Token Rules ---------------------------------------------------------------

Rules.

{IDENT}      : {token, {ident, TokenLine, list_to_atom(TokenChars)}}.
\:{IDENT}    : {token, {atom_lit, TokenLine, parse_atom(TokenChars)}}.
\:{STRING}   : {token, {atom_lit, TokenLine, parse_atom_quoted(TokenChars)}}.
{STRING}     : {token, {string_lit, TokenLine, parse_string(TokenChars)}}.
{WHITESPACE} : whitespace_token(TokenChars, TokenLine).
\+           : {token, {'+', TokenLine}}.
-            : {token, {'-', TokenLine}}.
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

%-- Helper Functions ----------------------------------------------------------

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

% Classify whitespace as being a newline or a space (skipped).
whitespace_token(Chars, Line) ->
  case re:run(Chars, "[\n;]") of
    nomatch -> skip_token;
    _       -> {token, {newline, Line}}
  end.
