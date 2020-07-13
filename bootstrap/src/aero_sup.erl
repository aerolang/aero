-module(aero_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

%% -----------------------------------------------------------------------------
%% Public API
%% -----------------------------------------------------------------------------

start_link() ->
  supervisor:start_link(?MODULE, []).

init(_Args) ->
  SupervisorSpecification = #{
    strategy => one_for_one
  },
  ChildSpecifications = [
    #{
      id => aero_compile_env,
      start => {aero_compile_env, start_link, []}
    }
  ],
  {ok, {SupervisorSpecification, ChildSpecifications}}.
