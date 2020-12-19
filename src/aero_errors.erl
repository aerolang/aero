%%% Displays various errors from the compiler.

-module(aero_errors).

-export([format_error/1]).

%% -----------------------------------------------------------------------------
%% Public API
%% -----------------------------------------------------------------------------

-spec format_error(term()) -> binary().
format_error(Error) ->
  Message =
    case message(Error) of
      {Format, Args} -> lists:flatten(io_lib:fwrite(Format, [io_lib:write(T) || T <- Args]));
      Format         -> Format
    end,
  list_to_binary(Message).

%% -----------------------------------------------------------------------------
%% Helper Functions
%% -----------------------------------------------------------------------------

%% Parser messages.
message(no_tokens) ->
  "no tokens provided to parser, at least EOF is required";
message({unexpected_token, Found}) ->
  {"unexpected token '~p'", [Found]};
message({expected_token, Expected, Found}) ->
  {"expected token '~p', found token '~p'", [Expected, Found]};
message({expected_token, Expected}) ->
  {"expected token '~p'", [Expected]};
message({nonassoc_op, Op}) ->
  {"operator token '~p' is not associative", [Op]};

%% Anything else...
message(_) ->
  "unknown error".
