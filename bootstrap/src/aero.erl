%%% Bootstrap compiler for the Aero programming language.

-module(aero).

-export([compile/1]).

%% -----------------------------------------------------------------------------
%% Public API
%% -----------------------------------------------------------------------------

compile(Filename) ->
  ContentResult = file:read_file(Filename),
  TokensResult = tokenize(ContentResult),
  AstResult = parse(TokensResult),
  TransformResult = transform(AstResult),
  output(TransformResult).

%% -----------------------------------------------------------------------------
%% Helper Functions
%% -----------------------------------------------------------------------------

%% Wrappers to allow for a compile pipeline operation.

tokenize({ok, Content}) ->
  case aero_lexer:tokenize(Content) of
    {ok, Tokens, _EndLine} -> {ok, Tokens};
    {error, _} = Error -> Error
  end;
tokenize({error, _} = Error) ->
  Error.

parse({ok, Tokens}) -> aero_parser:parse(Tokens);
parse({error, _} = Error) -> Error.

transform({ok, Ast}) -> {ok, aero_transform:transform(Ast)};
transform({error, _} = Error) -> Error.

output({ok, Transformed}) -> {ok, 'Elixir.Macro':to_string(Transformed)};
output({error, _} = Error) -> Error.
