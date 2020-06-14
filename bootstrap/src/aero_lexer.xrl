%%% Converts Aero source code into tokens.
%%%
%%% This uses leex to do the tokenizing and is converted to an .erl file.
%%% `aero_lexer:tokenize/1` is an exposed convenience function around the API
%%% that leex automatically generates.

%% -----------------------------------------------------------------------------
%% Regex Definitions
%% -----------------------------------------------------------------------------

Definitions.

INTEGER    = [0-9][0-9_]*
EXPONENT   = e[\+\-]?[0-9_]*[0-9][0-9_]*
FLOAT      = [0-9][0-9_]*(({EXPONENT})|(\.[0-9][0-9_]*({EXPONENT})?))
STRING     = "[^\"]*"
IDENT      = [a-zA-Z_][a-zA-Z0-9_]*
WHITESPACE = ([\s\t\r\n;]|(--[^\n]*))+

%% -----------------------------------------------------------------------------
%% Token Rules
%% -----------------------------------------------------------------------------

Rules.

%% Literals.
{INTEGER}    : integer_token(TokenChars, TokenLine).
{FLOAT}      : float_token(TokenChars, TokenLine).
\:{IDENT}    : atom_token(TokenChars, TokenLine).
\:{STRING}   : quoted_atom_token(TokenChars, TokenLine).
{STRING}     : string_token(TokenChars, TokenLine).

%% Identifiers.
{IDENT}      : ident_token(TokenChars, TokenLine).
{IDENT}\(    : ident_paren_token(TokenChars, TokenLine).
{IDENT}\[    : ident_brack_token(TokenChars, TokenLine).
'{IDENT}     : quote_ident_token(TokenChars, TokenLine).

%% Whitespace.
{WHITESPACE} : whitespace_token(TokenChars, TokenLine).

%% Containers.
\(           : {token, {op, TokenLine, '('}}.
\)           : {token, {op, TokenLine, ')'}}.
\{           : {token, {op, TokenLine, '{'}}.
\}           : {token, {op, TokenLine, '}'}}.
\[           : {token, {op, TokenLine, '['}}.
\]           : {token, {op, TokenLine, ']'}}.
#\(          : {token, {op, TokenLine, '#('}}.
#\{          : {token, {op, TokenLine, '#{'}}.
\.\{         : {token, {op, TokenLine, '.{'}}.
#\[          : {token, {op, TokenLine, '#['}}.
#!\[         : {token, {op, TokenLine, '#!['}}.
<<           : {token, {op, TokenLine, '<<'}}.
>>           : {token, {op, TokenLine, '>>'}}.

%% Arithmetic operators.
\+           : {token, {op, TokenLine, '+'}}.
-            : {token, {op, TokenLine, '-'}}.
\*           : {token, {op, TokenLine, '*'}}.
/            : {token, {op, TokenLine, '/'}}.
\%           : {token, {op, TokenLine, '%'}}.

%% Comparison and logical operators.
==           : {token, {op, TokenLine, '=='}}.
!=           : {token, {op, TokenLine, '!='}}.
<            : {token, {op, TokenLine, '<'}}.
>            : {token, {op, TokenLine, '>'}}.
<=           : {token, {op, TokenLine, '<='}}.
>=           : {token, {op, TokenLine, '>='}}.
<=>          : {token, {op, TokenLine, '<=>'}}.
&&           : {token, {op, TokenLine, '&&'}}.
\|\|         : {token, {op, TokenLine, '||'}}.

%% Bitwise operators.
&&&          : {token, {op, TokenLine, '&&&'}}.
\|\|\|       : {token, {op, TokenLine, '|||'}}.
\^\^\^       : {token, {op, TokenLine, '^^^'}}.
<<<          : {token, {op, TokenLine, '<<<'}}.
>>>          : {token, {op, TokenLine, '>>>'}}.
~~~          : {token, {op, TokenLine, '~~~'}}.

%% Misc. operators.
,            : {token, {op, TokenLine, ','}}.
\$           : {token, {op, TokenLine, '$'}}.
\:           : {token, {op, TokenLine, ':'}}.
\:\:         : {token, {op, TokenLine, '::'}}.
=            : {token, {op, TokenLine, '='}}.
->           : {token, {op, TokenLine, '->'}}.
<-           : {token, {op, TokenLine, '<-'}}.
=>           : {token, {op, TokenLine, '=>'}}.
\^           : {token, {op, TokenLine, '^'}}.
\*\*         : {token, {op, TokenLine, '**'}}.
\+\+         : {token, {op, TokenLine, '++'}}.
&            : {token, {op, TokenLine, '&'}}.
\|           : {token, {op, TokenLine, '|'}}.
\?           : {token, {op, TokenLine, '?'}}.
!            : {token, {op, TokenLine, '!'}}.
\?!          : {token, {op, TokenLine, '?!'}}.
\\\\         : {token, {op, TokenLine, '\\\\'}}.
\?\?         : {token, {op, TokenLine, '??'}}.
!!           : {token, {op, TokenLine, '!!'}}.
\?\.         : {token, {op, TokenLine, '?.'}}.
!\.          : {token, {op, TokenLine, '!.'}}.
in           : {token, {op, TokenLine, 'in'}}.
!in          : {token, {op, TokenLine, '!in'}}.
as           : {token, {op, TokenLine, 'as'}}.

%% Periods.
\.           : {token, {op, TokenLine, '.'}}.
\.\.         : {token, {op, TokenLine, '..'}}.
\.\.<        : {token, {op, TokenLine, '..<'}}.
\.\.\.       : {token, {op, TokenLine, '...'}}.
\.\.\.<      : {token, {op, TokenLine, '...<'}}.

%% -----------------------------------------------------------------------------
%% Helper Functions
%% -----------------------------------------------------------------------------

Erlang code.

-export([tokenize/1]).

%% Wrapped tokenize function for adding an EOF, exposed publicly.
tokenize(Input) ->
  case string(binary_to_list(Input)) of
    {ok, Tokens, EndLine} -> {ok, Tokens ++ [{eof, 0}], EndLine};
    {error, _, _} = Error -> Error
  end.

integer_token(Chars, Line) ->
  CharsCleaned = lists:filter(fun (Char) -> Char /= $_ end, Chars),
  {token, {integer_lit, Line, list_to_integer(CharsCleaned)}}.

float_token(Chars, Line) ->
  CharsCleaned = lists:filter(fun (Char) -> Char /= $_ end, Chars), 
  {token, {float_lit, Line, list_to_float(CharsCleaned)}}.

atom_token(Chars, Line) ->
  Atom = to_atom(Chars, 1, 0),
  {token, {atom_lit, Line, Atom}}.

quoted_atom_token(Chars, Line) ->
  Atom = to_atom(Chars, 2, 1),
  {token, {atom_lit, Line, Atom}}.

ident_token(Chars, Line) ->
  Ident = to_atom(Chars),
  {token, {ident, Line, Ident}}.

ident_paren_token(Chars, Line) ->
  Ident = to_atom(Chars, 0, 1),
  {token, {ident_paren, Line, Ident}, "("}.

ident_brack_token(Chars, Line) ->
  Ident = to_atom(Chars, 0, 1),
  {token, {ident_brack, Line, Ident}, "["}.

quote_ident_token(Chars, Line) ->
  Ident = to_atom(Chars, 1, 0),
  {token, {quote_ident, Line, Ident}}.

string_token(Chars, Line) ->
  String = list_to_binary(lists:sublist(Chars, 2, length(Chars) - 2)),
  {token, {string_lit, Line, String}}.

%% Classify whitespace as being a newline or a space (skipped).
whitespace_token(Chars, Line) ->
  case re:run(Chars, "[\n;]") of
    nomatch -> skip_token;
    _ -> {token, {newline, Line}}
  end.

%% Convert a charlist to an atom.
to_atom(Chars) ->
  list_to_atom(Chars).

%% Convert a charlist to an atom and trim off characters.
to_atom(Chars, PadLeft, PadRight) ->
  LeftIndex = 1 + PadLeft,
  RightIndex = length(Chars) - PadLeft - PadRight,
  Trimmed = lists:sublist(Chars, LeftIndex, RightIndex),
  to_atom(Trimmed).
