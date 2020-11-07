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
  list_to_binary(lists:flatten(String)).

%% -----------------------------------------------------------------------------
%% Helper Functions
%% -----------------------------------------------------------------------------

pprint(Node) ->
  pprint(Node, 0).

pprint({c_module, _, Name, Exports, _Attrs, Defs}, Level) ->
  ExportStrs = pprint_args(export, Exports, Level + 2),
  InnerDefs = [
    [pprint(FuncName), "\n", spaces(Level), pprint(Func, Level + 4)]
      || {FuncName, Func} <- Defs
  ],
  DefStrs = pprint_args(def, InnerDefs, Level + 2),
  format([module, Name, ExportStrs, [], DefStrs], Level);

pprint({c_integer, _, Integer}, _Level) ->
  integer_to_list(Integer);
pprint({c_float_lit, _, Float}, _Level) ->
  float_to_list(Float);
pprint({c_atom_lit, _, Atom}, _Level) ->
  Str = atom_to_list(Atom),
  Alnum = fun(C) -> C =:= $_ orelse (C >= $a andalso C =< $z)
                             orelse (C >= $A andalso C =< $Z)
                             orelse (C >= $0 andalso C =< $9) end,
  case lists:all(Alnum, Str) andalso hd(Str) > $9 of
    true  -> [":", Str];
    false -> [":\"", Str, "\""]
  end;
pprint({c_string_lit, _, String}, _Level) ->
  binary_to_list(String);

pprint({c_func, _, _Args, Ret, _Where, Body}, Level) ->
  RetStr = pprint_arg(ret, Ret, Level + 2),
  BodyStr = pprint_arg(body, Body, Level + 2),
  format([func, RetStr, BodyStr], Level);

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

% Keep binaries as they are, and otherwise, turn it into a binary.
pprint(Node, _Level) when is_list(Node) ->
  Node;
pprint(Node, _Level) ->
  io_lib:fwrite("~p", [Node]).

pprint_args(Label, Args, Level) ->
  [pprint_arg(Label, Arg, Level) || Arg <- Args].

pprint_arg(Label, Arg, Level) ->
  ["\n" | format([Label, Arg], Level)].

% S-expressions.
format(Nodes, Level) when is_list(Nodes) ->
  Formatted = [pprint(Node, Level) || Node <- Nodes],
  [spaces(Level), "(", lists:join(" ", Formatted), ")"].

spaces(Level) ->
  lists:duplicate(Level, " ").
