%%% Command-line entrypoint.

-module(aero_cli).

-export([main/1]).

%% -----------------------------------------------------------------------------
%% Public API
%% -----------------------------------------------------------------------------

%% Entrypoint.
main(["compile" | Args]) ->
  case parse_args(Args) of
    {ok, {Options, []}} ->
      Input = proplists:get_value(input, Options),
      case Input of
        undefined ->
          show_usage({missing_input, "Input file is required"});
        _ ->
          OutDir = proplists:get_value(out_dir, Options),
          Mode =
            case {proplists:get_bool(escript, Options), proplists:get_bool(core, Options)} of
              {false, false} -> beam;
              {true,  false} -> escript;
              {false, true}  -> core;
              {true,  true}  -> show_usage({bad_mode, "Cannot use both escript and core"})
            end,

          %% Configure the global environment.
          code:add_pathsa(proplists:get_all_values(path, Options)),

          Session = aero_session:new(Input, [
            {out_dir, OutDir},
            {mode, Mode}
          ]),

          %% Compile the root. Local modules that are dependents will be added
          %% during expansion.
          write_output(aero:compile(Input, Session))
      end;
    {error, {Reason, Data}} ->
      show_usage({Reason, Data})
  end;
main(_) ->
  show_usage().

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
  getopt:parse(getopt_spec(), Args).

%% Display how to use command-line interface and exit.
show_usage() ->
  getopt:usage(getopt_spec(), "aero compile"),
  halt(1).

%% Display usage with error message.
-dialyzer({no_return, show_usage/1}).
show_usage({Reason, Data}) ->
  print_error("Error: ~s ~p~n~n", [Reason, Data]),
  show_usage().

%% Options for getopt.
getopt_spec() ->
  [
    {out_dir, $o, "out-dir", {binary, <<"out">>}, "Output folder"},
    {escript, undefined, "escript", boolean, "Compile to an executable escript"},
    {path, $P, "add-path", binary, "Prepends a path to the Erlang code path"},
    {core, undefined, "core", boolean, "Compile to Core Aero"},
    {input, undefined, undefined, binary, "Input Aero file"}
  ].

print(Format, Data) ->
  io:format(Format, Data).

print_error(Format, Data) ->
  io:format(standard_error, Format, Data).
