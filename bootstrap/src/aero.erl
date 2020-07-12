%%% Bootstrap compiler for the Aero programming language.

-module(aero).

-export([compile/3]).

-export_type([options/0]).

%% -----------------------------------------------------------------------------
%% Public API
%% -----------------------------------------------------------------------------

-type option() :: {paths, [string()]}
                | {package, string()}.

-type options() :: [option()].

-spec compile(file:filename(), file:filename(), options()) -> ok.
compile(InputFile, OutDir, Options) ->
  InputResult = file:read_file(InputFile),
  TokensResult = tokenize(InputResult),
  AstResult = parse(TokensResult),
  TransformResult = transform(AstResult),
  ExFileResult = write_ex(TransformResult, InputFile, OutDir),
  ex_compile(ExFileResult, OutDir, Options).

%% -----------------------------------------------------------------------------
%% Helper Functions
%% -----------------------------------------------------------------------------

%% Wrappers to allow for a compile pipeline operation.

tokenize({ok, Input}) ->
  case aero_lexer:tokenize(Input) of
    {ok, Tokens, _EndLine} -> {ok, Tokens};
    {error, _} = Error -> Error
  end;
tokenize({error, _} = Error) ->
  Error.

parse({ok, Tokens}) -> aero_parser:parse(Tokens);
parse({error, _} = Error) -> Error.

transform({ok, Ast}) -> {ok, aero_transform:transform(Ast)};
transform({error, _} = Error) -> Error.

write_ex({ok, Transformed}, InputFile, OutDir) ->
  ExFile = filename:join([OutDir, "ex", filename:basename(InputFile, ".aero") ++ ".ex"]),
  ExSource = 'Elixir.Macro':to_string(Transformed),
  Formatted = 'Elixir.Code':'format_string!'(ExSource),
  case filelib:ensure_dir(ExFile) of
    ok ->
      case file:write_file(ExFile, Formatted) of
        ok -> {ok, ExFile};
        {error, _} = Error -> Error
      end;
    {error, _} = Error -> Error
  end;
write_ex({error, _} = Error, _, _) ->
  Error.

ex_compile({ok, ExFile}, OutDir, _Options) ->
  BeamDir = filename:join([OutDir, "ebin"]),
  case filelib:ensure_dir(filename:join([BeamDir, "."])) of
    ok ->
      case 'Elixir.Kernel.ParallelCompiler':compile_to_path([list_to_binary(ExFile)],
                                                            list_to_binary(BeamDir)) of
        {ok, _, _} -> ok;
        {error, Errors, Warnings} -> {Errors, Warnings}
      end;
    {error, _} = Error -> Error
  end;

ex_compile({error, _} = Error, _, _) ->
  Error.
