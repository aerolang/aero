-module(aero_session).
-behaviour(gen_server).

-export([start_link/0, stop/0]).
-export([configure/3, root/0, out_dir/0, pkg/0, visible_pkgs/0]).
-export([local_path/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% -----------------------------------------------------------------------------
%% Public API
%% -----------------------------------------------------------------------------

-record(state, {table}).

%% Interface.

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
  gen_server:call(?MODULE, stop).

%% Setup global information.
configure(Root, OutDir, Pkg) ->
  gen_server:call(?MODULE, {configure, {Root, OutDir, Pkg}}).

%% Root filename for compilation.
root() ->
  gen_server:call(?MODULE, root).

%% Output directory.
out_dir() ->
  gen_server:call(?MODULE, out_dir).

%% Current package name.
pkg() ->
  gen_server:call(?MODULE, pkg).

%% Map between available packages and their entrypoint modules.
visible_pkgs() ->
  gen_server:call(?MODULE, visible_pkgs).

%% Localize path to a file from the root directory.
local_path(Filename) ->
  case filename:extension(Filename) of
    <<".aero">> ->
      case root() of
        nil ->
          nil;
        Root ->
          remainder_filename(Filename, filename:dirname(Root))
      end;
    <<".ex">> ->
      case out_dir() of
        nil ->
          nil;
        OutDir ->
          remainder_filename(Filename, filename:join(OutDir, "ex"))
      end
  end.

%% gen_server callbacks.

init(_Args) ->
  Table = ets:new(?MODULE, [set, private]),
  {ok, #state{table = Table}}.

handle_call({configure, {Root, OutDir, Pkg}}, _From, State) ->
  true = ets:insert(State#state.table, [
    {root, Root},
    {out_dir, OutDir},
    {pkg, Pkg},
    {visible_pkgs, visible_pkgs(Pkg)}
  ]),
  {reply, ok, State};
handle_call(root, _From, State) ->
  {reply, fetch(State#state.table, root), State};
handle_call(out_dir, _From, State) ->
  {reply, fetch(State#state.table, out_dir), State};
handle_call(pkg, _From, State) ->
  {reply, fetch(State#state.table, pkg), State};
handle_call(visible_pkgs, _From, State) ->
  {reply, fetch(State#state.table, visible_pkgs, []), State};
handle_call(stop, _From, State) ->
  {stop, normal, stopped, State};
handle_call(Msg, _From, State) ->
  {stop, {bad_call, Msg}, State}.

handle_cast(Msg, State) ->
  {stop, {bad_cast, Msg}, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% -----------------------------------------------------------------------------
%% Helper Functions
%% -----------------------------------------------------------------------------

%% Get visible Aero packages from current package.
%% Adapted from code:all_available/0.
visible_pkgs(CurrPkg) ->
  case code:get_mode() of
    interactive ->
      visible_pkgs(CurrPkg, code:get_path(), []);
    embedded ->
      visible_pkgs(CurrPkg, [], [])
  end.

visible_pkgs(CurrPkg, [Path | Tail], Acc) ->
  case erl_prim_loader:list_dir(Path) of
    {ok, Files} ->
      visible_pkgs(CurrPkg, Tail, visible_pkgs(CurrPkg, Path, Files, Acc));
    _ ->
      visible_pkgs(CurrPkg, Tail, Acc)
  end;
visible_pkgs(CurrPkg, [], Acc) ->
  % Packages not in the path, but loaded into Erlang already.
  Loaded =
    lists:filtermap(fun({Module, _}) ->
      case visible_pkg(CurrPkg, atom_to_binary(Module, utf8)) of
        nil ->
          false;
        Pkg ->
          {true, Pkg}
        end
    end, code:all_loaded()),
  lists:umerge(lists:usort(Loaded), lists:usort(Acc)).

visible_pkgs(CurrPkg, Path, [File | Tail], Acc) ->
  case {filename:extension(File), filename:basename(File, <<".beam">>)} of
    {<<".beam">>, Module} ->
      case visible_pkg(CurrPkg, Module) of
        nil ->
          visible_pkgs(CurrPkg, Path, Tail, Acc);
        Pkg ->
          visible_pkgs(CurrPkg, Path, Tail, [Pkg | Acc])
      end;
    _ ->
      visible_pkgs(CurrPkg, Path, Tail, Acc)
  end;
visible_pkgs(_CurrPkg, _Path, [], Acc) ->
  Acc.

visible_pkg(CurrPkg, Module) ->
  % A module is visible when it's in the form x-y@1.2.3#x_y or x-y#x_y.
  case string:split(Module, "#") of
    [Left, Right] ->
      case {string:split(Left, "@"), string:replace(Right, "-", "_"), atom_to_list(CurrPkg)} of
        {[Name | Tail], [Name], CurrName} when length(Name) > 0, Name =/= CurrName,
                                               length(Tail) < 2 ->
          {binary_to_atom(Name, utf8), binary_to_atom(Module, utf8)};
        _ ->
          nil
      end;
    _ ->
      nil
  end.

%% Return part of input filename which doesn't match the root directory.
remainder_filename(Filename, RootDir) ->
  filename:join(
    remainder_filename_split(
      filename:split(filename:absname(Filename)),
      filename:split(filename:absname(RootDir))
    )
  ).

remainder_filename_split([Head | FilenameTail], [Head | RootDirTail]) ->
  remainder_filename_split(FilenameTail, RootDirTail);
remainder_filename_split(Filename, _) ->
  Filename.

%% Lookup a key in ETS.
fetch(Table, Key) ->
  fetch(Table, Key, nil).

fetch(Table, Key, Default) ->
  case ets:lookup(Table, Key) of
    [{Key, Value}] ->
      Value;
    _ ->
      Default
  end.
