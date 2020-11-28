%%% Bootstrap compiler for the Aero programming language.

-module(aero).

-export([compile/2]).

%% -----------------------------------------------------------------------------
%% Public API
%% -----------------------------------------------------------------------------

compile(InputFile, Options) ->
  Passes = lists:flatten([
    {fun aero_scan:scan/1, []},
    {fun aero_parse:parse/1, []},
    {fun aero_expand:expand/2, [aero_context:new(InputFile)]},
    {fun aero_resolve:resolve/1, []},

    case {proplists:get_bool(core, Options), proplists:get_bool(escript, Options)} of
      {false, false} ->
        [
          {fun aero_codegen:generate/1, []},
          {fun write_beam/1, []}
        ];
      {false, true} ->
        [
          {fun aero_codegen:generate/1, []},
          {fun write_escript/1, []}
        ];
      {true, _} ->
        [
          {fun write_core/1, []}
        ]
    end
  ]),
  transform(file:read_file(InputFile), Passes).

%% -----------------------------------------------------------------------------
%% Helper Functions
%% -----------------------------------------------------------------------------

transform({ok, _} = Result, [])             -> Result;
transform({ok, Data}, [{Fun, Args} | Tail]) -> transform(apply(Fun, [Data | Args]), Tail);
transform({error, _} = Error, _)            -> Error.

%% Write BEAM files to the output directory.
write_beam(Modules) ->
  BeamDir = filename:join([aero_session:out_dir(), <<"ebin">>]),

  case filelib:ensure_dir(filename:join([BeamDir, <<".">>])) of
    ok                 -> write_beam(Modules, BeamDir, 0);
    {error, _} = Error -> Error
  end.

write_beam([], _, Acc) ->
  {ok, Acc};
write_beam([{Name, Beam} | T], BeamDir, Acc) ->
  Filename = filename:join([BeamDir, atom_to_list(Name) ++ ".beam"]),

  case file:write_file(Filename, Beam) of
    ok                 -> write_beam(T, BeamDir, Acc + 1);
    {error, _} = Error -> Error
  end.

%% Create an escript in the output directory.
write_escript(Modules) ->
  BinDir = filename:join([aero_session:out_dir(), <<"bin">>]),
  Filename = filename:join([BinDir, [filename:basename(aero_session:root(), <<".aero">>)]]),

  % The root module is passed as the first result and we use that to find the
  % main function. The entry point does some conversions and is where the
  % escript will start.
  {Main, _} = hd(Modules),
  AllModules = [aero_codegen:generate_entry_point(Main) | Modules],

  case filelib:ensure_dir(filename:join([BinDir, <<".">>])) of
    ok ->
      escript:create(Filename, [
        shebang,
        {emu_args, "-escript main entrypoint"},
        {archive, [{atom_to_list(Name) ++ ".beam", Beam} || {Name, Beam} <- AllModules], []}
      ]),

      case file:change_mode(Filename, 8#755) of
        ok ->
          {ok, length(Modules)};
        {error, _} = Error ->
          Error
      end;
    {error, _} = Error ->
      Error
  end.

%% Write Core Aero into the output directory instead if requested.
write_core({c_pkg, _, _, CoreModules} = Package) ->
  CoreDir = filename:join([aero_session:out_dir(), <<"core">>]),
  Names = [Name || {c_mod, _, {c_path, _, [{c_var, _, Name}]}, _, _} <- CoreModules],
  Strings = aero_core_pprint:pprint(Package),
  Modules = lists:zip(Names, Strings),

  case filelib:ensure_dir(filename:join([CoreDir, <<".">>])) of
    ok                 -> write_core(Modules, CoreDir, 0);
    {error, _} = Error -> Error
  end.

write_core([], _, Acc) ->
  {ok, Acc};
write_core([{Name, String} | T], CoreDir, Acc) ->
  Filename = filename:join([CoreDir, atom_to_list(Name) ++ ".c-aero"]),

  case file:write_file(Filename, <<String/binary, "\n">>) of
    ok                 -> write_core(T, CoreDir, Acc + 1);
    {error, _} = Error -> Error
  end.
