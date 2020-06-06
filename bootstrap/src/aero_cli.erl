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
      Output = proplists:get_value(output, Options),
      case {Input, Output} of
        {undefined, _} ->
          show_usage({missing_input, "Input file is required"});
        {_, undefined} ->
          show_usage({missing_output, "Output file is required"});
        _ ->
          CompiledResult = aero:compile(Input),
          write_output(Output, CompiledResult)
      end;
    {error, {Reason, Data}} ->
      show_usage({Reason, Data})
  end.

%% -----------------------------------------------------------------------------
%% Helper Functions
%% -----------------------------------------------------------------------------

%% Write output to file, handle errors in compiling or file writing and halt.
write_output(Output, {ok, Compiled}) ->
  case file:write_file(Output, Compiled) of
    ok ->
      ok;
    {error, Reason} ->
      print_error("Error writing to output file: ~p~n", [Reason]),
      halt(1)
  end;
write_output(_Output, {error, _} = Error) ->
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
    {output, $o,        "output",  string, "Output Elixir file"},
    {input,  undefined, undefined, string, "Input Aero file"}
  ].

print_error(Format, Data) ->
  io:format(standard_error, Format, Data).
