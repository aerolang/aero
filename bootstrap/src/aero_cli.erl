%%% Command-line entrypoint.

-module(aero_cli).

-export([main/1]).

%% -----------------------------------------------------------------------------
%% Public API
%% -----------------------------------------------------------------------------

%% Entrypoint.
main(Args) ->
  case parse_args(Args) of
    {ok, {Options, []}} ->
      Input = proplists:get_value(input, Options),
      case Input of
        undefined ->
          show_usage({missing_input, "Input file is required"});
        _ ->
          OutDir = proplists:get_value(out_dir, Options),
          CompileOptions = [
            {paths, proplists:get_all_values(path, Options)},
            {pkg, proplists:get_value(pkg, Options, false)},
            {root, true}
          ],
          CompileResult = aero:compile(Input, OutDir, CompileOptions),
          write_output(CompileResult)
      end;
    {error, {Reason, Data}} ->
      show_usage({Reason, Data})
  end.

%% -----------------------------------------------------------------------------
%% Helper Functions
%% -----------------------------------------------------------------------------

%% Write output to file, handle errors in compiling or file writing and halt.
write_output({ok, _}) ->
  ok;
write_output({error, _} = Error) ->
  print_error("Compile error: ~p~n", [Error]),
  halt(1).

%% Use getopt to parse arguments.
parse_args(Args) ->
  ListArgs = lists:map(fun binary_to_list/1, Args),
  getopt:parse(getopt_spec(), ListArgs).

%% Display how to use command-line interface and terminate.
show_usage() ->
  getopt:usage(getopt_spec(), "aero_bootstrap"),
  halt(1).

%% Display usage with error message.
show_usage({Reason, Data}) ->
  print_error("Error: ~s ~p~n~n", [Reason, Data]),
  show_usage().

%% Options for getopt.
getopt_spec() ->
  [
    {out_dir, $o, "out-dir", {string, "out"}, "Output folder"},
    {path, $P, "add-path", string, "Prepends a path to the Erlang code path"},
    {pkg, undefined, "pkg", undefined, "Compile as a package"},
    {input, undefined, undefined, string, "Input Aero file"}
  ].

print_error(Format, Data) ->
  io:format(standard_error, Format, Data).
