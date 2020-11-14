%%% Aero Pretty Printer.
%%%
%%% Prints out Core Aero in S-Expression form to be analyzed easier than looking
%%% at the data structures that represent it directly.

-module(aero_pprint).

-export([pprint_core_aero/1]).

%% -----------------------------------------------------------------------------
%% Public API
%% -----------------------------------------------------------------------------

-spec pprint_core_aero(aero_expander:c_any()) -> binary().
pprint_core_aero(CoreAero) ->
  String = pprint(CoreAero),
  re:replace(String, " +\\n", "\n", [global, {return, binary}]).

%% -----------------------------------------------------------------------------
%% Helper Functions
%% -----------------------------------------------------------------------------

pprint(Node) ->
  pprint(Node, 0).

pprint({c_module, _, Name, _Attrs, Defs}, Level) ->
  InnerDefs =
    lists:map(fun({DefName, Vis, Expr}) ->
      [pprint(Vis), " ", pprint(DefName), "\n", spaces(Level + 4), pprint(Expr, Level + 4)]
    end, Defs),
  DefStrs = lists:join($\n, pprint_args(def, InnerDefs, Level + 2)),
  format([module, Name, [], DefStrs], Level);

pprint(c_vis_pub, _Level) ->
  "pub";
pprint(c_vis_priv, _Level) ->
  "priv";

pprint({c_block, _, Exprs}, Level) ->
  ExprStrs = [["\n", spaces(Level + 2), pprint(Expr, Level + 4)] || Expr <- Exprs],
  format([block | ExprStrs], Level);

pprint({c_bool_lit, _, Bool}, _Level) ->
  atom_to_list(Bool);
pprint({c_int_lit, _, Integer}, _Level) ->
  integer_to_list(Integer);
pprint({c_float_lit, _, Float}, _Level) ->
  float_to_list(Float);
pprint({c_atom_lit, _, Atom}, _Level) ->
  [$:, printable_atom(Atom)];
pprint({c_str_lit, _, String}, _Level) ->
  [$", printable_string(String), $"];

pprint({c_unit, _}, _Level) ->
  "unit";

pprint({c_tuple, _, Exprs}, Level) ->
  format([tuple | Exprs], Level + 2);

pprint({c_cons, _, Head, Tail}, Level) ->
  format([cons, Head, Tail], Level + 2);
pprint({c_nil, _}, _Level) ->
  "nil";

pprint({c_dict, _, Pairs}, Level) ->
  InnerPairs =
    lists:map(fun({Key, Value}) ->
      [pprint(Key, Level + 4), " ", pprint(Value, Level + 4)]
    end, Pairs),
  PairStrs = pprint_args(pair, InnerPairs, Level + 2),
  format([dict, PairStrs], Level);

pprint({c_func, _, Args, Result, _Where, Body}, Level) ->
  InnerArgs =
    lists:map(fun({ArgVar, ArgType}) ->
      [pprint(ArgVar), " ", pprint(ArgType)]
    end, Args),
  ArgStrs = pprint_args(arg, InnerArgs, Level + 2),
  ResultStr = pprint_arg(result, Result, Level + 2),
  BodyStr = pprint_arg(body, Body, Level + 2),
  format([func, ArgStrs, ResultStr, BodyStr], Level);

pprint({c_call, _, Callee, Args}, Level) ->
  ArgStrs = pprint_args(arg, Args, Level + 2),
  format([call, Callee | ArgStrs], Level);
pprint({c_apply, _, Callee, Args}, Level) ->
  ArgStrs = pprint_args(arg, Args, Level + 2),
  format([apply, Callee | ArgStrs], Level);

pprint({c_var, _, Name}, _Level) ->
  [$%, printable_atom(Name)];

pprint({c_path, _, Segments}, _Level) ->
  SegmentStrs = [printable_atom(Name) || {c_var, _, Name} <- Segments],
  [$$ | lists:join("::", SegmentStrs)]; 

pprint({c_let, _, Left, Type, Right}, Level) ->
  format(['let', Left, Type, Right], Level);
pprint({c_letrec, _, Left, Type, Right}, Level) ->
  format([letrec, Left, Type, Right], Level);

pprint(c_type_bool, _Level) ->
  "bool";
pprint(c_type_int, _Level) ->
  "int";
pprint(c_type_float, _Level) ->
  "float";
pprint(c_type_atom, _Level) ->
  "atom";
pprint(c_type_str, _Level) ->
  "str";
pprint(c_type_bytes, _Level) ->
  "bytes";
pprint(c_type_bits, _Level) ->
  "bits";
pprint(c_type_ref, _Level) ->
  "ref";

pprint(c_type_unit, _Level) ->
  "unit";

pprint({c_type_tuple, TArgs}, Level) ->
  format([tuple | TArgs], Level + 2);
pprint({c_type_list, T}, Level) ->
  format([list, T], Level + 2);
pprint({c_type_dict, K, V}, Level) ->
  format([dict, K, V], Level + 2);

pprint({c_type_func, TArgs, TResult}, Level) ->
  format([func | TArgs] ++ [TResult], Level + 2);

pprint(c_type_wld, _Level) ->
  "wld";
pprint(c_type_never, _Level) ->
  "never";
pprint({c_type_mbox, T}, Level) ->
  format([mbox, T], Level + 2);
pprint({c_type_addr, T}, Level) ->
  format([addr, T], Level + 2);

pprint({c_type_param, Name}, _Level) ->
  [$', printable_atom(Name)];
pprint({c_type_tag, Name}, _Level) ->
  [$:, printable_atom(Name)];

pprint({c_type_struct, Name, TArgs}, Level) ->
  format([struct, Name | pprint_args(arg, TArgs, Level + 4)], Level + 2);
pprint({c_type_proto, Name, TArgs}, Level) ->
  format([proto, Name| pprint_args(arg, TArgs, Level + 4)], Level + 2);
pprint({c_type_union, TArgs}, Level) ->
  TArgStrs = [[$\n, spaces(Level + 2), pprint(TArg, Level + 4)] || TArg <- TArgs],
  format([union | TArgStrs], Level + 2);
pprint({c_type_inter, TArgs}, Level) ->
  TArgStrs = [[$\n, spaces(Level + 2), pprint(TArg, Level + 4)] || TArg <- TArgs],
  format([inter | TArgStrs], Level + 2);

%% Keep binaries as they are, and otherwise, turn it into a binary.
pprint(Node, _Level) when is_list(Node) ->
  Node;
pprint(Node, _Level) when is_atom(Node) ->
  io_lib:fwrite("~s", [Node]);
pprint(Node, _Level) ->
  io_lib:fwrite("~p", [Node]).

pprint_args(Label, Args, Level) ->
  [pprint_arg(Label, Arg, Level) || Arg <- Args].

pprint_arg(Label, Arg, Level) ->
  ["\n", spaces(Level) | format([Label, Arg], Level)].

%% S-expressions.
format(Nodes, Level) when is_list(Nodes) ->
  Formatted = [pprint(Node, Level) || Node <- Nodes],
  ["(", lists:join(" ", Formatted), ")"].

spaces(Level) ->
  lists:duplicate(Level, " ").

printable_atom(Atom) ->
  Str = atom_to_list(Atom),
  case lists:all(fun unquoted_atom_char/1, Str) of
    true  -> Str;
    false -> [$\", printable_string(Str), $\"]
  end.

%% Core Aero atoms also allow "-" and "." where Aero ones do not and can start
%% with a number as well.
unquoted_atom_char($_) ->
  true;
unquoted_atom_char($-) ->
  true;
unquoted_atom_char($.) ->
  true;
unquoted_atom_char(C) when C >= $a, C =< $z; C >= $A, C =< $Z; C >= $0, C =< $9 ->
  true;
unquoted_atom_char(_) ->
  false.

%% Convert to printable ASCII and escape characters.
printable_string(String) ->
  lists:map(fun(C) ->
    case C of
      0                        -> "\\0";
      7                        -> "\\a";
      $\b                      -> "\\b";
      27                       -> "\\e";
      $\f                      -> "\\f";
      $\n                      -> "\\n";
      $\r                      -> "\\r";
      $\t                      -> "\\t";
      $\v                      -> "\\v";
      $\\                      -> "\\\\";
      $\"                      -> "\\\"";
      _ when C >= $\s, C =< $~ -> C;
      _ when C < 16#FF         -> ["\\x", base16_pad(C, 2)];
      _ when C < 16#FFFF       -> ["\\u{", base16_pad(C, 4), "}"];
      _                        -> ["\\u{", base16_pad(C, 6), "}"]
    end
  end, unicode:characters_to_list(String)).

base16_pad(Integer, Length) ->
  string:pad(string:lowercase(integer_to_list(Integer, 16)), Length, leading, $0).
