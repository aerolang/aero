-module(aero_lexer).

-export([tokenize/1]).
-export_type([token/0]).

%% -----------------------------------------------------------------------------
%% Public API
%% -----------------------------------------------------------------------------

-type token() :: {integer_lit, meta(), integer()}
               | {float_lit, meta(), float()}
               | {atom_lit, meta(), atom()}
               | {string_lit, meta(), binary()}
               | {ident, meta(), atom()}
               | {type_param, meta(), atom()}
               | {blank, meta()}
               | {op, meta(), atom()}
               | {space, meta()}
               | {newline, meta()}
               | {eof, meta()}.

-type meta() :: [term()].

-spec tokenize(binary()) -> {ok, [token()], pos_integer()} | {error, term()}.
tokenize(Input) ->
  tokenize(string:to_graphemes(Input), {0, 1, 1}, []).

%% -----------------------------------------------------------------------------
%% Tokenizing
%% -----------------------------------------------------------------------------

-define(is_digit(S1), (S1 >= $0 andalso S1 =< $9)).
-define(is_hex(S1), (?is_digit(S1) orelse (S1 >= $a andalso S1 =< $f) orelse
                                          (S1 >= $A andalso S1 =< $F))).
-define(is_oct(S1), (S1 >= $0 andalso S1 =< $8)).
-define(is_bin(S1), (S1 =:= $0 orelse S1 =:= $1)).

-define(is_ident_start(S1), (S1 =:= $_ orelse (S1 >= $a andalso S1 =< $z) orelse
                                              (S1 >= $A andalso S1 =< $Z))).
-define(is_ident_continue(S1), (?is_ident_start(S1) orelse ?is_digit(S1))).

-define(is_whitespace_start_1(S), S =:= "\s"; S =:= "\t"; S =:= "\n"; S =:= ["\r\n"]; S =:= ";").
-define(is_whitespace_start_2(S), S =:= "//").

-define(is_op_1(S), S =:= "("; S =:= ")"; S =:= "{"; S =:= "}"; S =:= "["; S =:= "]"; S =:= "+";
                    S =:= "-"; S =:= "*"; S =:= "/"; S =:= "%"; S =:= "<"; S =:= ">"; S =:= ",";
                    S =:= "$"; S =:= ":"; S =:= "="; S =:= "^"; S =:= "&"; S =:= "|"; S =:= "?";
                    S =:= "!"; S =:= ".").
-define(is_op_2(S), S =:= "#("; S =:= "#{"; S =:= ".{"; S =:= "#["; S =:= "<<"; S =:= ">>";
                    S =:= "=="; S =:= "!="; S =:= "<="; S =:= ">="; S =:= "&&"; S =:= "||";
                    S =:= "::"; S =:= "->"; S =:= "<-"; S =:= "=>"; S =:= "++"; S =:= "?!";
                    S =:= "\\\\"; S =:= "??"; S =:= "!!"; S =:= "?."; S =:= "!."; S =:= "..").
-define(is_op_3(S), S =:= "#!["; S =:= "<=>"; S =:= "&&&"; S =:= "|||"; S =:= "^^^"; S =:= "<<<";
                    S =:= ">>>"; S =:= "~~~"; S =:= "..."; S =:= "..<").
-define(is_op_4(S), S =:= "...<").

tokenize(Input, Pos, Tokens) ->
  case next_token(Input, Pos) of
    {token, Rest, NewPos, Token}        -> tokenize(Rest, NewPos, [Token | Tokens]);
    {end_token, {_, EndLine, _}, Token} -> {ok, lists:reverse([Token | Tokens]), EndLine};
    {error, Error}                      -> {error, Error}
  end.

%% Numeric literals.
next_token("0b" ++ Cont, Pos) ->
  {Bin, Rest} = lists:splitwith(fun(S1) -> ?is_bin(S1) orelse S1 =:= $_ end, Cont),
  integer_token(Rest, Pos, Bin, 2, "0b");
next_token("0o" ++ Cont, Pos) ->
  {Oct, Rest} = lists:splitwith(fun(S1) -> ?is_oct(S1) orelse S1 =:= $_ end, Cont),
  integer_token(Rest, Pos, Oct, 8, "0o");
next_token("0x" ++ Cont, Pos) ->
  {Hex, Rest} = lists:splitwith(fun(S1) -> ?is_hex(S1) orelse S1 =:= $_ end, Cont),
  integer_token(Rest, Pos, Hex, 16, "0x");
next_token([S1 | _] = Input, Pos) when ?is_digit(S1) ->
  % A single `.` and `e` result in a float being made. Double `..` is tokenized
  % as an integer for a range.
  case lists:splitwith(fun(S2) -> ?is_digit(S2) orelse S2 =:= $_ end, Input) of
    {Int, [$., $. | _] = Rest} -> integer_token(Rest, Pos, Int, 10, "");
    {IntPart, [$. | _] = Rest} -> float_token(Rest, Pos, IntPart);
    {IntPart, [$e | _] = Rest} -> float_token(Rest, Pos, IntPart);
    {Int, Rest}                -> integer_token(Rest, Pos, Int, 10, "")
  end;

%% Atoms.
next_token([$:, S2 | _] = Input, Pos) when ?is_ident_start(S2); S2 =:= $" ->
  atom_token(Input, Pos);

%% Strings.
next_token([$" | _] = Input, Pos) ->
  string_token(Input, Pos);

%% Identifiers, type parameters, blanks, and operator-like identifiers.
next_token([S1 | _] = Input, Pos) when ?is_ident_start(S1) ->
  case ident_token(Input, Pos) of
    {token, Rest, _, {ident, _, '_'}} ->
      blank_token(Rest, Pos);
    {token, Rest, _, {ident, _, Ident}} when Ident =:= 'if'; Ident =:= else; Ident =:= for;
                                             Ident =:= while ->
      op_token(Rest, Pos, Ident, length(atom_to_list(Ident)));
    {token, _, _, _} = IdentToken ->
      IdentToken
  end;
next_token([$', S2 | _] = Input, Pos) when ?is_ident_start(S2) ->
  type_param_token(Input, Pos);

%% Whitespace.
next_token([S1, S2 | _] = Input, Pos) when ?is_whitespace_start_2([S1, S2]) ->
  whitespace_token(Input, Pos);
next_token([S1 | _] = Input, Pos) when ?is_whitespace_start_1([S1]) ->
  whitespace_token(Input, Pos);

%% Operators.
next_token([S1, S2, S3, S4 | Rest], Pos) when ?is_op_4([S1, S2, S3, S4]) ->
  op_token(Rest, Pos, list_to_atom([S1, S2, S3, S4]), 4);
next_token([S1, S2, S3 | Rest], Pos) when ?is_op_3([S1, S2, S3]) ->
  op_token(Rest, Pos, list_to_atom([S1, S2, S3]), 3);
next_token([S1, S2 | Rest], Pos) when ?is_op_2([S1, S2]) ->
  op_token(Rest, Pos, list_to_atom([S1, S2]), 2);
next_token([S1 | Rest], Pos) when ?is_op_1([S1]) ->
  op_token(Rest, Pos, list_to_atom([S1]), 1);

%% Eof.
next_token([], Pos) ->
  eof_token(Pos);

%% Anything else.
next_token([S | _], Pos) ->
  {error, {unexpected_char, unicode:characters_to_binary([S]), Pos}}.

%% -----------------------------------------------------------------------------
%% Token Types
%% -----------------------------------------------------------------------------

integer_token(Rest, Pos, Source, Base, Prefix) ->
  Length = length(Source) + length(Prefix),
  NewPos = shift(Pos, Length, 0, Length),
  case drop_underscores(Source) of
    [] when Rest =:= [] ->
      {error, {unexpected_eof, Pos}};
    Filtered when Filtered =:= []; ?is_ident_start(hd(Rest)) ->
      % No numbers after underscore, unexpected alpha, or number out of range.
      {error, {unexpected_char, unicode:characters_to_binary([hd(Rest)]), NewPos}};
    Filtered ->
      {token, Rest, NewPos, {integer_lit, meta(Pos, Length), list_to_integer(Filtered, Base)}}
  end.

float_token([$. | Cont], Pos, IntSource) ->
  {FractSource, Rest} = lists:splitwith(fun(S1) -> ?is_digit(S1) orelse S1 =:= $_ end, Cont),
  Length = length(IntSource) + length(FractSource) + 1,
  NewPos = shift(Pos, Length, 0, Length),

  case drop_underscores(FractSource) of
    [] when Rest =:= [] ->
      {error, {unexpected_eof, Pos}};
    FractFiltered when FractFiltered =:= []; ?is_ident_start(hd(Rest)), hd(Rest) =/= $e ->
      % No numbers after underscore, unexpected alpha, or number out of range.
      {error, {unexpected_char, unicode:characters_to_binary([hd(Rest)]), NewPos}};
    FractFiltered when Rest =:= []; hd(Rest) =/= $e ->
      % No exponent: simple float.
      FloatFiltered = drop_underscores(IntSource) ++ "." ++ FractFiltered,
      {token, Rest, NewPos, {float_lit, meta(Pos, Length), list_to_float(FloatFiltered)}};
    _->
      % Continuing with exponent.
      float_token(Rest, Pos, IntSource, "." ++ FractSource)
  end;
float_token([$e | _] = Rest, Pos, IntSource) ->
  float_token(Rest, Pos, IntSource, "").

% Adding on exponent (with "e" in front).
float_token([$e, $+ | Cont], Pos, IntSource, FractSource) ->
  float_token(Cont, Pos, IntSource, FractSource, "e+");
float_token([$e, $- | Cont], Pos, IntSource, FractSource) ->
  float_token(Cont, Pos, IntSource, FractSource, "e-");
float_token([$e | Cont], Pos, IntSource, FractSource) ->
  float_token(Cont, Pos, IntSource, FractSource, "e").

float_token(Rest, Pos, IntSource, FractSource, SignSource) ->
  {ExpSource, Rest2} = lists:splitwith(fun(S1) -> ?is_digit(S1) orelse S1 =:= $_ end, Rest),
  Length = length(IntSource) + length(FractSource) + length(SignSource) + length(ExpSource),
  NewPos = shift(Pos, Length, 0, Length),

  case drop_underscores(ExpSource) of
    [] when Rest =:= [] ->
      {error, {unexpected_eof, Pos}};
    ExpFiltered when ExpFiltered =:= []; ?is_ident_start(hd(Rest2)) ->
      % No numbers after underscore, unexpected alpha, or number out of range.
      {error, {unexpected_char, unicode:characters_to_binary([hd(Rest2)]), NewPos}};
    ExpFiltered ->
      % Erlang float parser needs a fractional part.
      Mantissa =
        case FractSource of
          "" -> drop_underscores(IntSource) ++ ".0";
          _  -> drop_underscores(IntSource ++ FractSource)
        end,
      FloatFiltered = Mantissa ++ SignSource ++ ExpFiltered,
      {token, Rest2, NewPos, {float_lit, meta(Pos, Length), list_to_float(FloatFiltered)}}
  end.

atom_token([$: | Cont], Pos) when hd(Cont) =:= $" ->
  {token, Rest, NewPos, {string_lit, Meta, String}} = string_token(Cont, shift(Pos, 1, 0, 1)),
  {token, Rest, NewPos, {atom_lit, meta(Pos, span_size(Meta) + 1), binary_to_atom(String, utf8)}};
atom_token([$: | Cont], Pos) ->
  {token, Rest, NewPos, {ident, Meta, Ident}} = ident_token(Cont, shift(Pos, 1, 0, 1)),
  {token, Rest, NewPos, {atom_lit, meta(Pos, span_size(Meta) + 1), Ident}}.

string_token([$" | Cont], Pos) ->
  string_token(Cont, shift(Pos, 1, 0, 1), Pos, "").

string_token(Input, {Index, _, _} = Pos, {StartIndex, _, _} = StartPos, Acc) ->
  case Input of
    [$" | Rest] ->
      NewPos = shift(Pos, 1, 0, 1),
      Meta = meta(StartPos, Index + 1 - StartIndex),
      {token, Rest, NewPos, {string_lit, Meta, unicode:characters_to_binary(lists:reverse(Acc))}};
    [$\\, $x, S3, S4 | Cont] when ?is_hex(S3), ?is_hex(S4) ->
      case escape_unicode([S3, S4]) of
        none      -> {error, {invalid_str_escape, <<"x">>, shift(Pos, 1, 0, 1)}};
        [Escaped] -> string_token(Cont, shift(Pos, 4, 0, 4), StartPos, [Escaped | Acc])
      end;
    [$\\, $u, ${, S4, S5, S6, S7, $} | Cont] when ?is_hex(S4); ?is_hex(S5); ?is_hex(S6);
                                                  ?is_hex(S7) ->
      case escape_unicode([S4, S5, S6, S7]) of
        none      -> {error, {invalid_str_escape, <<"u">>, shift(Pos, 1, 0, 1)}};
        [Escaped] -> string_token(Cont, shift(Pos, 8, 0, 8), StartPos, [Escaped | Acc])
      end;
    [$\\, $u, ${, S4, S5, S6, S7, S8, S9, $} | Cont] when ?is_hex(S4); ?is_hex(S5); ?is_hex(S6);
                                                          ?is_hex(S7); ?is_hex(S8); ?is_hex(S9) ->
      case escape_unicode([S4, S5, S6, S7, S8, S9]) of
        none      -> {error, {invalid_str_escape, <<"u">>, shift(Pos, 1, 0, 1)}};
        [Escaped] -> string_token(Cont, shift(Pos, 10, 0, 10), StartPos, [Escaped | Acc])
      end;
    [$\\, S2 | Cont] ->
      case escape_char(S2) of
        none ->
          {error, {invalid_str_escape, unicode:characters_to_binary([S2]), shift(Pos, 1, 0, 1)}};
        Escaped ->
          string_token(Cont, shift(Pos, 2, 0, 2), StartPos, [Escaped | Acc])
      end;
    [S1 | _] when S1 =:= $\n; S1 =:= "\r\n" ->
      {error, {unexpected_char, unicode:characters_to_binary([S1]), Pos}};
    [S1 | Cont] ->
      string_token(Cont, shift(Pos, input_size([S1]), 0, 1), StartPos, [S1 | Acc]);
    [] ->
      {error, {unexpected_eof, Pos}}
  end.

ident_token([S1 | Cont], Pos) when ?is_ident_start(S1) ->
  {Tail, Rest} = lists:splitwith(fun(S2) -> ?is_ident_continue(S2) end, Cont),

  Ident = list_to_atom(unicode:characters_to_list([S1 | Tail])),
  Length = length([S1 | Tail]),
  Size = input_size([S1 | Tail]),
  NewPos = shift(Pos, Size, 0, Length),

  {token, Rest, NewPos, {ident, meta(Pos, Size), Ident}}.

type_param_token([$' | Cont], Pos) ->
  {token, Rest, NewPos, {ident, Meta, Ident}} = ident_token(Cont, Pos),
  {token, Rest, NewPos, {type_param, meta(Pos, span_size(Meta)), Ident}}.

blank_token(Rest, Pos) ->
  {token, Rest, shift(Pos, 1, 0, 1), {blank, meta(Pos, 1)}}.

op_token(Rest, Pos, Op, Length) ->
  {token, Rest, shift(Pos, Length, 0, Length), {op, meta(Pos, Length), Op}}.

whitespace_token(Input, Pos) ->
  whitespace_token(Input, Pos, Pos, none).

whitespace_token(Input, {Index, _, _} = Pos, {StartIndex, _, _} = StartPos, Type) ->
  case Input of
    [$\s | Rest] ->
      whitespace_token(Rest, shift(Pos, 1, 0, 1), StartPos, Type);
    [$\t | Rest] ->
      whitespace_token(Rest, shift(Pos, 1, 0, 2), StartPos, Type);
    [$\n | Rest] when Type =:= semicolon ->
      whitespace_token(Rest, shift(Pos, 1, 1, 0), StartPos, semicolon);
    [$\n | Rest] ->
      whitespace_token(Rest, shift(Pos, 1, 1, 0), StartPos, newline);
    ["\r\n" | Rest] when Type =:= semicolon ->
      whitespace_token(Rest, shift(Pos, 2, 1, 0), StartPos, semicolon);
    ["\r\n" | Rest] ->
      whitespace_token(Rest, shift(Pos, 2, 1, 0), StartPos, newline);
    [$\\, $\n | Rest] ->
      whitespace_token(Rest, shift(Pos, 2, 1, 0), StartPos, Type);
    [$\\, "\r\n" | Rest] ->
      whitespace_token(Rest, shift(Pos, 3, 1, 0), StartPos, Type);
    [$; | Rest] ->
      whitespace_token(Rest, shift(Pos, 1, 0, 1), StartPos, semicolon);
    [$/, $/ | Cont] ->
      {Trimmed, Rest} = lists:splitwith(fun(S) -> S =/= $\n andalso S =/= "\r\n" end, Cont),
      NewPos = shift(Pos, input_size(Trimmed) + 2, 0, length(Trimmed) + 2),
      whitespace_token(Rest, NewPos, StartPos, Type);
    _ when Type =:= semicolon; Type =:= newline, (Input =:= [] orelse hd(Input) =/= $|) ->
      {token, Input, Pos, {newline, meta(StartPos, Index - StartIndex)}};
    _ ->
      {token, Input, Pos, {space, meta(StartPos, Index - StartIndex)}}
  end.

eof_token(Pos) ->
  {end_token, Pos, {eof, meta(Pos, 0)}}.

%% -----------------------------------------------------------------------------
%% Utilities
%% -----------------------------------------------------------------------------

drop_underscores(Source) ->
  lists:filter(fun(S1) -> S1 =/= $_ end, Source).

escape_char($0)  -> 0;
escape_char($a)  -> 7;
escape_char($b)  -> $\b;
escape_char($e)  -> 27;
escape_char($f)  -> $\f;
escape_char($n)  -> $\n;
escape_char($r)  -> $\r;
escape_char($t)  -> $\t;
escape_char($v)  -> $\v;
escape_char($\\) -> $\\;
escape_char($")  -> $\";
escape_char(_)   -> none.

escape_unicode(Input) ->
  case unicode:characters_to_list([list_to_integer(Input, 16)]) of
    {error, _, _}      -> none;
    {incomplete, _, _} -> none;
    String             -> String
  end.

input_size(Input) ->
  byte_size(unicode:characters_to_binary(Input)).

shift({Index, Line, Column}, IndexIncr, 0, ColumnIncr) ->
  {Index + IndexIncr, Line, Column + ColumnIncr};
shift({Index, Line, _Column}, IndexIncr, LineIncr, 0) ->
  {Index + IndexIncr, Line + LineIncr, 1}.

meta({Index, Line, Column}, Length) ->
  [
    {line, Line},
    {column, Column},
    {span, aero_span:new(Index, Index + Length)}
  ].

span_size(Meta) ->
  Span = proplists:get_value(span, Meta),
  aero_span:stop(Span) - aero_span:start(Span).
