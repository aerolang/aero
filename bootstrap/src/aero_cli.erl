%%% Command-line entrypoint.

-module(aero_cli).

-export([main/1]).

%% -----------------------------------------------------------------------------
%% Public API
%% -----------------------------------------------------------------------------

%% Entrypoint.
main([<<"compile">> | Args]) ->
  case parse_args(Args) of
    {ok, {Options, []}} ->
      Input = proplists:get_value(input, Options),
      case Input of
        undefined ->
          show_usage({missing_input, "Input file is required"}),
          halt(1);
        _ ->
          OutDir = proplists:get_value(out_dir, Options),
          Escript = proplists:get_bool(escript, Options),
          Pkg = proplists:get_value(pkg, Options),
          Core = proplists:get_bool(core, Options),

          %% Configure the global environment.
          code:add_pathsa(proplists:get_all_values(path, Options)),
          aero_session:configure(Input, OutDir, Pkg),

          %% Compile the root. Local modules that are dependents will be added
          %% during expansion.
          write_output(aero:compile(Input, [{escript, Escript}, {core, Core}]))
      end;
    {error, {Reason, Data}} ->
      show_usage({Reason, Data}),
      halt(1)
  end;
main(_) ->
  show_usage(),
  halt(1).

%% -----------------------------------------------------------------------------
%% Helper Functions
%% -----------------------------------------------------------------------------

%% Write output to file, handle errors in compiling or file writing and halt.
write_output({ok, Number}) ->
  print("Compiled ~p module(s).~n", [Number]);
write_output({error, _} = Error) ->
  print_error("Compile error: ~p~n", [Error]),
  halt(1).

%% Use getopt to parse arguments.
parse_args(Args) ->
  ListArgs = lists:map(fun binary_to_list/1, Args),
  getopt:parse(getopt_spec(), ListArgs).

%% Display how to use command-line interface.
show_usage() ->
  getopt:usage(getopt_spec(), "aero_bootstrap compile").

%% Display usage with error message.
show_usage({Reason, Data}) ->
  print_error("Error: ~s ~p~n~n", [Reason, Data]),
  show_usage().

%% Options for getopt.
getopt_spec() ->
  [
    {out_dir, $o, "out-dir", {binary, <<"out">>}, "Output folder"},
    {escript, undefined, "escript", boolean, "Compile to an executable escript"},
    {path, $P, "add-path", binary, "Prepends a path to the Erlang code path"},
    {pkg, undefined, "pkg", {atom, aero}, "Package name"},
    {core, undefined, "core", boolean, "Compile to Core Aero"},
    {input, undefined, undefined, binary, "Input Aero file"}
  ].

print(Format, Data) ->
  io:format(Format, Data).

print_error(Format, Data) ->
  io:format(standard_error, Format, Data).
