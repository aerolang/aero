%%% Bootstrap compiler for the Aero programming language.

-module(aero).

-export([compile/2]).

%% -----------------------------------------------------------------------------
%% Public API
%% -----------------------------------------------------------------------------

%% Compile an Aero file.
-spec compile(binary(), aero_session:t()) -> {ok, non_neg_integer()} | {error, term()}.
compile(InputFile, Session) ->
  Passes = lists:flatten([
    {fun aero_scan:scan/1, []},
    {fun aero_parse:parse/1, []},
    {fun aero_expand:expand/2, [Session]},
    {fun aero_lift:lift/1, []},
    {fun aero_resolve:resolve/1, []},

    case aero_session:mode(Session) of
      beam ->
        [
          {fun aero_codegen:generate/1, []},
          {fun write_beam/2, [Session]}
        ];
      escript ->
        [
          {fun aero_codegen:generate/1, []},
          {fun write_escript/2, [Session]}
        ];
      core ->
        [
          {fun write_core/2, [Session]}
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
write_beam(Modules, Session) ->
  BeamDir = filename:join([aero_session:out_dir(Session), <<"ebin">>]),

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
write_escript(Modules, Session) ->
  BinDir = filename:join([aero_session:out_dir(Session), <<"bin">>]),
  Filename = filename:join([BinDir, atom_to_binary(aero_session:pkg(Session))]),

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
write_core({c_pkg, _, _, CoreModules} = Package, Session) ->
  CoreDir = filename:join([aero_session:out_dir(Session), <<"core">>]),
  Basenames = lists:map(fun core_filename/1, CoreModules),

  Strings = aero_core_pprint:pprint(Package),
  Modules = lists:zip(Basenames, Strings),

  case filelib:ensure_dir(filename:join([CoreDir, <<".">>])) of
    ok                 -> write_core(Modules, CoreDir, 0);
    {error, _} = Error -> Error
  end.

write_core([], _, Acc) ->
  {ok, Acc};
write_core([{Basename, String} | T], CoreDir, Acc) ->
  case file:write_file(filename:join([CoreDir, Basename]), <<String/binary, "\n">>) of
    ok                 -> write_core(T, CoreDir, Acc + 1);
    {error, _} = Error -> Error
  end.

core_filename({c_mod, _, {c_path, _, Vars}, _, _}) ->
  lists:join($., [Name || {c_var, _, Name} <- Vars]) ++ ".acore".
