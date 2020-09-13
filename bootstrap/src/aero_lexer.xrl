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
SPACE      = (([\s\t\r]*)|(//[^\n]*)|(\\\n))+
SPACE_CONT = [\n]([\s\t\r\n]|(//[^\n]*))*[\|]
NEWLINE    = ({SPACE})[\n;]([\s\t\r\n;]|(//[^\n]*))*

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

%% Identifier-like operators.
if           : {token, {op, TokenLine, 'if'}}.
else         : {token, {op, TokenLine, else}}.
for          : {token, {op, TokenLine, for}}.
while        : {token, {op, TokenLine, while}}.

%% Identifiers.
{IDENT}      : ident_token(TokenChars, TokenLine).
'{IDENT}     : type_param_token(TokenChars, TokenLine).

%% Whitespace.
{SPACE}      : {token, {space, TokenLine}}.
{SPACE_CONT} : {skip_token, [lists:last(TokenChars)]}.
{NEWLINE}    : {token, {newline, TokenLine}}.

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

%% Periods.
\.           : {token, {op, TokenLine, '.'}}.
\.\.         : {token, {op, TokenLine, '..'}}.
\.\.<        : {token, {op, TokenLine, '..<'}}.
\.\.\.       : {token, {op, TokenLine, '...'}}.
\.\.\.<      : {token, {op, TokenLine, '...<'}}.

%% Errors.
.            : {error, list_to_binary(io_lib:format("unexpected character '~s'", [TokenChars]))}.

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

type_param_token(Chars, Line) ->
  TypeParam = to_atom(Chars, 1, 0),
  {token, {type_param, Line, TypeParam}}.

string_token(Chars, Line) ->
  String = list_to_binary(lists:sublist(Chars, 2, length(Chars) - 2)),
  {token, {string_lit, Line, String}}.

%% Convert a charlist to an atom.
to_atom(Chars) ->
  list_to_atom(Chars).

%% Convert a charlist to an atom and trim off characters.
to_atom(Chars, PadLeft, PadRight) ->
  LeftIndex = 1 + PadLeft,
  RightIndex = length(Chars) - PadLeft - PadRight,
  Trimmed = lists:sublist(Chars, LeftIndex, RightIndex),
  to_atom(Trimmed).
