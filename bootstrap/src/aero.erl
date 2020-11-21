%%% Bootstrap compiler for the Aero programming language.

-module(aero).

-export([compile/2]).

%% -----------------------------------------------------------------------------
%% Public API
%% -----------------------------------------------------------------------------

compile(InputFile, Options) ->
  InputResult = file:read_file(InputFile),
  TokensResult = tokenize(InputResult),
  AstResult = parse(TokensResult),
  ExpandResult = expand(AstResult, InputFile),
  case proplists:get_bool(core, Options) of
    false -> codegen(ExpandResult);
    true  -> write_core(ExpandResult)
  end.

%% -----------------------------------------------------------------------------
%% Helper Functions
%% -----------------------------------------------------------------------------

%% Wrappers to allow for a compile pipeline operation.

tokenize({ok, Input})        -> aero_lexer:tokenize(Input);
tokenize({error, _} = Error) -> Error.

parse({ok, Tokens})       -> aero_parser:parse(Tokens);
parse({error, _} = Error) -> Error.

expand({ok, Ast}, InputFile)  -> aero_expander:expand(Ast, aero_context:new(InputFile));
expand({error, _} = Error, _) -> Error.

codegen({ok, CoreModules}) ->
  BeamDir = filename:join([aero_session:out_dir(), <<"ebin">>]),
  case filelib:ensure_dir(filename:join([BeamDir, <<".">>])) of
    ok                 -> codegen(CoreModules, BeamDir, 0);
    {error, _} = Error -> Error
  end;
codegen({error, _} = Error) ->
  Error.

codegen([], _, Acc) ->
  {ok, Acc};
codegen([H | T], BeamDir, Acc) ->
  case aero_codegen:generate(H) of
    {ok, Name, Module} ->
      Filename = filename:join([BeamDir, atom_to_list(Name) ++ ".beam"]),
      case file:write_file(Filename, Module) of
        ok                 -> codegen(T, BeamDir, Acc + 1);
        {error, _} = Error -> Error
      end;
    {error, _} = Error -> Error
  end.

write_core({ok, CoreModules}) ->
  BeamDir = filename:join([aero_session:out_dir(), <<"core">>]),
  case filelib:ensure_dir(filename:join([BeamDir, <<".">>])) of
    ok                 -> write_core(CoreModules, BeamDir, 0);
    {error, _} = Error -> Error
  end;
write_core({error, _} = Error) ->
  Error.

write_core([], _, Acc) ->
  {ok, Acc};
write_core([{c_module, _, {c_path, _, [{c_var, _, Name}]}, _, _} = H | T], BeamDir, Acc) ->
  String = aero_pprint:pprint_core_aero(H),
  Filename = filename:join([BeamDir, atom_to_list(Name) ++ ".c-aero"]),
  case file:write_file(Filename, <<String/binary, "\n">>) of
    ok                 -> write_core(T, BeamDir, Acc + 1);
    {error, _} = Error -> Error
  end.
