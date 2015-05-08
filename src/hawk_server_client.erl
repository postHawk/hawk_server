%% @author maximilian
%% @doc @todo Add description to hawk_server_client.


-module(hawk_server_client).
-behaviour(supervisor).
-include("env.hrl").
-include("mac.hrl").

-export([init/1]).
-export([start_link/1]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([]).



%% ====================================================================
%% Behavioural functions 
%% ====================================================================

start_link(Name) ->
    supervisor:start_link(?MODULE, [Name]).

init([Name]) ->
	gproc:reg_or_locate({n, l, Name}),
    {ok,
        {_SupFlags = {simple_one_for_one, ?MAX_RESTART, ?MAX_TIME},
            [
              % TCP Client
              {   undefined,                               % Id       = internal id
                  {hawk_server_chat_worker, start_link,[]},                  % StartFun = {M, F, A}
                  temporary,                               % Restart  = permanent | transient | temporary
                  2000,                                    % Shutdown = brutal_kill | int() >= 0 | infinity
                  worker,                                  % Type     = worker | supervisor
                  []                                       % Modules  = [Module] | dynamic
              }
            ]
        }
    }.

%% ====================================================================
%% Internal functions
%% ====================================================================

