%% @author Maximilian
%% @doc @todo Add description to hawk_server_api_sup.


-module(hawk_server_api_sup).
-behaviour(supervisor).
-include("mac.hrl").
-export([init/1]).

-export([start_link/0, get_worker/0]).

start_link() ->
   supervisor:start_link({local, ?MODULE}, ?MODULE, []).

get_worker() ->
	supervisor:start_child(?MODULE, []).

init([]) ->
	process_flag(trap_exit, true),
    {ok,
        {_SupFlags = {simple_one_for_one, ?MAX_RESTART, ?MAX_TIME},
            [
              % hawk_server_api_worker
              {   undefined,                               % Id       = internal id
                  {hawk_server_api_worker, start_link,[]},                  % StartFun = {M, F, A}
                  temporary,                               % Restart  = permanent | transient | temporary
                  2000,                                    % Shutdown = brutal_kill | int() >= 0 | infinity
                  worker,                                  % Type     = worker | supervisor
                  [hawk_server_api_worker]                                       % Modules  = [Module] | dynamic
              }
            ]
        }
    }.

