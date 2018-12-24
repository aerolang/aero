%-- Regex Definitions for Rules -----------------------------------------------

Definitions.

NUMERIC    = \.?[0-9][a-zA-Z0-9_]*
STRING     = "[^\"]*"
IDENT      = [a-zA-Z_][a-zA-Z0-9_]*
WHITESPACE = ([\s\t\r\n;]|(--[^\n]*))+

%-- Token Rules ---------------------------------------------------------------

Rules.

% Literals.
{NUMERIC}    : numeric_token(TokenChars, TokenLine).
\:{IDENT}    : atom_token(TokenChars, TokenLine).
\:{STRING}   : quoted_atom_token(TokenChars, TokenLine).
{STRING}     : string_token(TokenChars, TokenLine).

% Identifiers.
{IDENT}      : ident_token(TokenChars, TokenLine).
{IDENT}\:    : tag_token(TokenChars, TokenLine).
{STRING}\:   : quoted_tag_token(TokenChars, TokenLine).
{IDENT}\(    : ident_paren_token(TokenChars, TokenLine).
{STRING}\(   : quoted_ident_paren_token(TokenChars, TokenLine).
{IDENT}\[    : ident_indx_token(TokenChars, TokenLine).
{STRING}\[   : quoted_ident_indx_token(TokenChars, TokenLine).

% Whitespace.
{WHITESPACE} : whitespace_token(TokenChars, TokenLine).

% Brackets.
\(           : {token, {'(', TokenLine}}.
\)           : {token, {')', TokenLine}}.
\[           : {token, {'[', TokenLine}}.
\]           : {token, {']', TokenLine}}.
\{           : {token, {'{', TokenLine}}.
\}           : {token, {'}', TokenLine}}.
#\(          : {token, {'#(', TokenLine}}.
#\[          : {token, {'#[', TokenLine}}.
#\{          : {token, {'#{', TokenLine}}.
<<           : {token, {'<<', TokenLine}}.
>>           : {token, {'>>', TokenLine}}.

% Arithmetic operators.
\+           : {token, {'+', TokenLine}}.
-            : {token, {'-', TokenLine}}.
\*           : {token, {'*', TokenLine}}.
/            : {token, {'/', TokenLine}}.
\%           : {token, {'%', TokenLine}}.
\^           : {token, {'^', TokenLine}}.

% Comparison operators.
==           : {token, {'==', TokenLine}}.
!=           : {token, {'!=', TokenLine}}.
<            : {token, {'<', TokenLine}}.
>            : {token, {'>', TokenLine}}.
<=           : {token, {'<=', TokenLine}}.
>=           : {token, {'>=', TokenLine}}.

% Bitwise operators.
&&&          : {token, {'&&&', TokenLine}}.
|||          : {token, {'|||', TokenLine}}.
\^\^\^       : {token, {'^^^', TokenLine}}.
<<<          : {token, {'<<<', TokenLine}}.
>>>          : {token, {'>>>', TokenLine}}.
~~~          : {token, {'~~~', TokenLine}}.

% Misc. operators.
,            : {token, {',', TokenLine}}.
\:\:         : {token, {'::', TokenLine}}.
=            : {token, {'=', TokenLine}}.
->           : {token, {'->', TokenLine}}.
=>           : {token, {'=>', TokenLine}}.
\*\*         : {token, {'**', TokenLine}}.
&            : {token, {'&', TokenLine}}.
|            : {token, {'|', TokenLine}}.
@            : {token, {'@', TokenLine}}.
@!           : {token, {'@!', TokenLine}}.
!            : {token, {'!', TokenLine}}.
\?           : {token, {'?', TokenLine}}.
!!           : {token, {'!!', TokenLine}}.
\?\?         : {token, {'??', TokenLine}}.
\?\.         : {token, {'?.', TokenLine}}.

% Periods.
\.           : {token, {'.', TokenLine}}.
\.\.         : {token, {'..', TokenLine}}.
\.\.\.       : {token, {'...', TokenLine}}.

%-- Helper Functions ----------------------------------------------------------

Erlang code.

numeric_token(Chars, Line) ->
  % TODO: Will need to handle floats as well.
  case parse_integer(Chars) of
    {ok, {Type, Integer}} -> {token, {Type, Line, Integer}};
    {error, Msg}          -> {error, Msg}
  end.

atom_token(Chars, Line) ->
  Atom = to_atom(Chars, 1, 0),
  {token, {atom_lit, Line, Atom}}.

quoted_atom_token(Chars, Line) ->
  Atom = to_atom(Chars, 2, 1),
  {token, {atom_lit, Line, Atom}}.

tag_token(Chars, Line) ->
  Tag = to_atom(Chars, 0, 1),
  {token, {tag, Line, Tag}}.

quoted_tag_token(Chars, Line) ->
  Tag = to_atom(Chars, 1, 2),
  {token, {tag, Line, Tag}}.

ident_paren_token(Chars, Line) ->
  Ident = to_atom(Chars, 0, 1),
  {token, {ident_call, Line, Ident}, "("}.

quoted_ident_paren_token(Chars, Line) ->
  Ident = to_atom(Chars, 1, 2),
  {token, {ident_call, Line, Ident}, "("}.

ident_indx_token(Chars, Line) ->
  Ident = to_atom(Chars, 0, 1),
  {token, {ident_indx, Line, Ident}, "["}.

quoted_ident_indx_token(Chars, Line) ->
  Ident = to_atom(Chars, 1, 2),
  {token, {ident_indx, Line, Ident}, "["}.

ident_token(Chars, Line) ->
  Ident = to_atom(Chars),
  {token, {ident, Line, Ident}}.

string_token(Chars, Line) ->
  String = list_to_binary(lists:sublist(Chars, 2, length(Chars) - 2)),
  {token, {string_lit, Line, String}}.

% Classify whitespace as being a newline or a space (skipped).
whitespace_token(Chars, Line) ->
  case re:run(Chars, "[\n;]") of
    nomatch -> skip_token;
    _       -> {token, {newline, Line}}
  end.

parse_integer(Chars) ->
  case match_integer(Chars) of
    {ok, NumberLen, TypeLen} ->
      Number = parse_integer_number(lists:sublist(Chars, 1, NumberLen)),
      Type = lists:sublist(Chars, length(Chars) - TypeLen, TypeLen),

      % TODO: Need to check if the numbers are in valid ranges.
      Token = case Type of
        "i8"    -> {i8_lit, Number};
        "i16"   -> {i16_lit, Number};
        "i32"   -> {i32_lit, Number};
        "i64"   -> {i64_lit, Number};
        "isize" -> {isize_lit, Number};
        "u8"    -> {u8_lit, Number};
        "u16"   -> {u16_lit, Number};
        "u32"   -> {u32_lit, Number};
        "u64"   -> {u64_lit, Number};
        "usize" -> {usize_lit, Number};
        ""      -> {integer_lit, Number}
      end,

      {ok, Token};
    {error, Msg} ->
      {error, Msg}
  end.

match_integer(Chars) ->
  case re:run(Chars, "^([0-9][0-9_]*)([iu](?:8|16|32|64|size))?$") of
    {match, [_, {_, NumberLen}, {_, TypeLen}]} -> {ok, NumberLen, TypeLen};
    {match, [_, {_, NumberLen}]}               -> {ok, NumberLen, 0};
    nomatch -> {error, "invalid integer literal syntax in \"" ++ Chars ++ "\""}
  end.

% Remove underscores from a charlist and make it a number.
parse_integer_number(Chars) ->
  Filtered = lists:filter(fun (Char) -> Char /= $_ end, Chars),
  list_to_integer(Filtered).

% Convert a charlist to an atom.
to_atom(Chars) ->
  list_to_atom(Chars).

% Convert a charlist to an atom and trim off characters.
to_atom(Chars, PadLeft, PadRight) ->
  LeftIndex = 1 + PadLeft,
  RightIndex = length(Chars) - PadLeft - PadRight,
  Trimmed = lists:sublist(Chars, LeftIndex, RightIndex),
  to_atom(Trimmed).
