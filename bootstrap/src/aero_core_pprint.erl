%%% Core Aero pretty printer.
%%%
%%% Prints out Core Aero in S-Expression form to be analyzed easier than looking
%%% at the data structures that represent it directly.

-module(aero_core_pprint).

-export([pprint/1]).

%% -----------------------------------------------------------------------------
%% Public API
%% -----------------------------------------------------------------------------

-spec pprint(aero_core:c_pkg()) -> [binary()].
pprint({c_pkg, _, _, Modules}) ->
  [re:replace(pprint(Module, 0), " +\\n", "\n", [global, {return, binary}]) || Module <- Modules].

%% -----------------------------------------------------------------------------
%% Helper Functions
%% -----------------------------------------------------------------------------

%% Definitions.

pprint({c_mod, _, Name, _Attrs, Defs}, Level) ->
  DefStrs = lists:join($\n, pprint_args(Defs, Level)),
  format([module, Name, [], DefStrs], Level);

pprint({c_def_func, _, Path, Vis, Func}, Level) ->
  format([def, Vis, func, Path, pprint_arg(Func, Level)], Level);
pprint({c_def_const, _, Path, Vis, Type, Expr}, Level) ->
  format([def, Vis, const, Path, pprint_arg(Type, Level), pprint_arg(Expr, Level)], Level);

pprint(c_vis_pub, _Level) ->
  "pub";
pprint(c_vis_priv, _Level) ->
  "priv";

%% Expressions.

pprint({c_block, _, Exprs}, Level) ->
  format([block | pprint_args(Exprs, Level)], Level);

pprint({c_bool, _, Bool}, _Level) ->
  atom_to_list(Bool);
pprint({c_int, _, Integer}, _Level) ->
  integer_to_list(Integer);
pprint({c_float, _, Float}, _Level) ->
  float_to_list(Float);
pprint({c_atom, _, Atom}, _Level) ->
  [$:, printable_atom(Atom)];
pprint({c_str, _, String}, _Level) ->
  [$", printable_string(String), $"];

pprint({c_unit, _}, _Level) ->
  "unit";

pprint({c_tuple, _, Exprs}, Level) ->
  format([tuple | Exprs], Level);

pprint({c_cons, _, Head, Tail}, Level) ->
  format([cons, Head, Tail], Level);
pprint({c_nil, _}, _Level) ->
  "nil";

pprint({c_dict, _, Pairs}, Level) ->
  PairStrs = pprint_args(pair, format_inner(Pairs, Level), Level),
  format([dict, PairStrs], Level);

pprint({c_func, _, Args, Result, _Where, Body}, Level) ->
  ArgStrs = pprint_args(arg, format_inner(Args, Level), Level),
  ResultStr = pprint_arg(result, Result, Level),
  BodyStr = pprint_arg(body, Body, Level),
  format([func, ArgStrs, ResultStr, BodyStr], Level);

pprint({c_call, _, Callee, Args}, Level) ->
  format([call, Callee | pprint_args(arg, Args, Level)], Level);
pprint({c_apply, _, Callee, Args}, Level) ->
  format([apply, Callee | pprint_args(arg, Args, Level)], Level);

pprint({c_var, _, Name}, _Level) ->
  [$%, printable_atom(Name)];

pprint({c_path, _, Vars}, _Level) ->
  [$$ | lists:join("::", [printable_atom(Name) || {c_var, _, Name} <- Vars])];

pprint({c_let, _, Left, Type, Right}, Level) ->
  format(['let', Left, Type, Right], Level);
pprint({c_letrec, _, Left, Type, Right}, Level) ->
  format([letrec, Left, Type, Right], Level);

pprint({c_match, _, Expr, Cases}, Level) ->
  ExprStr = pprint_arg(expr, Expr, Level),
  CaseStrs = pprint_args('case', [pprint_args(Case, Level + 2) || Case <- Cases], Level),
  format([match, ExprStr | CaseStrs], Level);

pprint({c_args, _, Args}, Level) ->
  format([args | Args], Level);

%% Patterns.

pprint({c_pat_args, _, PatArgs}, Level) ->
  format([args | PatArgs], Level);

pprint({c_pat_bool, _, Bool}, _Level) ->
  atom_to_list(Bool);
pprint({c_pat_int, _, Integer}, _Level) ->
  integer_to_list(Integer);
pprint({c_pat_float, _, Float}, _Level) ->
  float_to_list(Float);
pprint({c_pat_atom, _, Atom}, _Level) ->
  [$:, printable_atom(Atom)];
pprint({c_pat_str, _, String}, _Level) ->
  [$", printable_string(String), $"];

pprint({c_pat_unit, _}, _Level) ->
  "unit";

pprint({c_pat_tuple, _, Pats}, Level) ->
  format([tuple | Pats], Level);

pprint({c_pat_cons, _, Head, Tail}, Level) ->
  format([cons, Head, Tail], Level);
pprint({c_pat_nil, _}, _Level) ->
  "nil";

pprint({c_pat_dict, _, Pairs}, Level) ->
  PairStrs = pprint_args(pair, format_inner(Pairs, Level), Level),
  format([dict, PairStrs], Level);

pprint({c_pat_var, _, Name}, _Level) ->
  [$%, printable_atom(Name)];

%% Types.

pprint({c_type_bool, _}, _Level) ->
  "bool";
pprint({c_type_int, _}, _Level) ->
  "int";
pprint({c_type_float, _}, _Level) ->
  "float";
pprint({c_type_atom, _}, _Level) ->
  "atom";
pprint({c_type_str, _}, _Level) ->
  "str";
pprint({c_type_bytes, _}, _Level) ->
  "bytes";
pprint({c_type_bits, _}, _Level) ->
  "bits";
pprint({c_type_ref, _}, _Level) ->
  "ref";

pprint({c_type_unit, _}, _Level) ->
  "unit";

pprint({c_type_tuple, _, TArgs}, Level) ->
  format([tuple | TArgs], Level);
pprint({c_type_list, _, T}, Level) ->
  format([list, T], Level);
pprint({c_type_dict, _, K, V}, Level) ->
  format([dict, K, V], Level);

pprint({c_type_func, _, TArgs, TResult}, Level) ->
  format([func | TArgs] ++ [TResult], Level);

pprint({c_type_wld, _}, _Level) ->
  "wld";
pprint({c_type_never, _}, _Level) ->
  "never";
pprint({c_type_mbox, _, T}, Level) ->
  format([mbox, T], Level);
pprint({c_type_addr, _, T}, Level) ->
  format([addr, T], Level);

pprint({c_type_var, _, Name}, _Level) ->
  [$', printable_atom(Name)];
pprint({c_type_path, _, TypeVars}, _Level) ->
  [$$ | lists:join("::", [printable_atom(Name) || {c_type_var, _, Name} <- TypeVars])];
pprint({c_type_tag, _, Name}, _Level) ->
  [$:, printable_atom(Name)];

pprint({c_type_struct, _, Path, TArgs}, Level) ->
  format([struct, Path | pprint_args(arg, TArgs, Level)], Level);
pprint({c_type_proto, _, Path, TArgs}, Level) ->
  format([proto, Path| pprint_args(arg, TArgs, Level)], Level);
pprint({c_type_union, _, TArgs}, Level) ->
  format([union | pprint_args(TArgs, Level)], Level);
pprint({c_type_inter, _, TArgs}, Level) ->
  format([inter | pprint_args(TArgs, Level)], Level);

%% Utilities.

%% Keep binaries as they are, and otherwise, turn it into a binary.
pprint(Node, _Level) when is_list(Node) ->
  Node;
pprint(Node, _Level) when is_atom(Node) ->
  io_lib:fwrite("~s", [Node]);
pprint(Node, _Level) ->
  io_lib:fwrite("~p", [Node]).

%% Print arguments each on a new line.
pprint_args(Args, Level) when is_tuple(Args) ->
  pprint_args(tuple_to_list(Args), Level);
pprint_args(Args, Level) ->
  [pprint_arg(Arg, Level) || Arg <- Args].

pprint_arg(Arg, Level) ->
  ["\n", spaces(Level + 2) | pprint(Arg, Level + 2)].

%% Print arguments each on a new line with a label.
pprint_args(Label, Args, Level) when is_tuple(Args) ->
  pprint_args(Label, tuple_to_list(Args), Level);
pprint_args(Label, Args, Level) ->
  [pprint_arg(Label, Arg, Level) || Arg <- Args].

pprint_arg(Label, Arg, Level) ->
  ["\n", spaces(Level + 2) | format([Label, Arg], Level + 2)].

%% S-expressions.
format(Nodes, Level) when is_list(Nodes) ->
  Formatted = [pprint(Node, Level) || Node <- Nodes],
  ["(", lists:join(" ", Formatted), ")"].

%% Print inner tuples expressions to be formatted inside an S-expression.
format_inner(Nodes, Level) ->
  [lists:join(" ", [pprint(E, Level + 2) || E <- tuple_to_list(Node)]) || Node <- Nodes].

spaces(Level) ->
  lists:duplicate(Level, $\s).

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
