%%% Bootstrap compiler for the Aero programming language.

-module(aero).

-export([compile/2]).

%% -----------------------------------------------------------------------------
%% Public API
%% -----------------------------------------------------------------------------

compile(InputFile, Options) ->
  Passes = [
    {fun aero_lexer:tokenize/1, []},
    {fun aero_parser:parse/1, []},
    {fun aero_expander:expand/2, [aero_context:new(InputFile)]},
    {fun aero_resolver:resolve/1, []},

    case proplists:get_bool(core, Options) of
      false -> {fun write_beam/1, []};
      true  -> {fun write_core/1, []}
    end
  ],
  transform(file:read_file(InputFile), Passes).

%% -----------------------------------------------------------------------------
%% Helper Functions
%% -----------------------------------------------------------------------------

transform({ok, _} = Result, [])             -> Result;
transform({ok, Data}, [{Fun, Args} | Tail]) -> transform(apply(Fun, [Data | Args]), Tail);
transform({error, _} = Error, _)            -> Error.

%% Write BEAM files to the output directory.
write_beam(CoreModules) ->
  BeamDir = filename:join([aero_session:out_dir(), <<"ebin">>]),

  case filelib:ensure_dir(filename:join([BeamDir, <<".">>])) of
    ok                 -> write_beam(CoreModules, BeamDir, 0);
    {error, _} = Error -> Error
  end.

write_beam([], _, Acc) ->
  {ok, Acc};
write_beam([H | T], BeamDir, Acc) ->
  case aero_codegen:generate(H) of
    {ok, Name, Module} ->
      Filename = filename:join([BeamDir, atom_to_list(Name) ++ ".beam"]),

      case file:write_file(Filename, Module) of
        ok                 -> write_beam(T, BeamDir, Acc + 1);
        {error, _} = Error -> Error
      end;
    {error, _} = Error -> Error
  end.

%% Write Core Aero into the output directory instead if requested.
write_core(CoreModules) ->
  BeamDir = filename:join([aero_session:out_dir(), <<"core">>]),

  case filelib:ensure_dir(filename:join([BeamDir, <<".">>])) of
    ok                 -> write_core(CoreModules, BeamDir, 0);
    {error, _} = Error -> Error
  end.

write_core([], _, Acc) ->
  {ok, Acc};
write_core([{c_module, _, {c_path, _, [{c_var, _, Name}]}, _, _} = H | T], BeamDir, Acc) ->
  String = aero_pprint:pprint_core_aero(H),
  Filename = filename:join([BeamDir, atom_to_list(Name) ++ ".c-aero"]),

  case file:write_file(Filename, <<String/binary, "\n">>) of
    ok                 -> write_core(T, BeamDir, Acc + 1);
    {error, _} = Error -> Error
  end.
