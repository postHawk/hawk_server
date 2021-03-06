
-module(hawk_server_sup).

-behaviour(supervisor).

-include("env.hrl").
-include("mac.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([init/1]).
-export([start_link/0]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ====================================================================
%% Behavioural functions 
%% ====================================================================

init([]) ->
    process_flag(trap_exit, true),

    Modules = [
        % queue server
        {   hawk_server_queue,
            {hawk_server_queue,start_link,[]},
            permanent,                               % Restart  = permanent | transient | temporary
            2000,                                % Shutdown = brutal_kill | int() >= 0 | infinity
            worker,                              % Type     = worker | supervisor
            [hawk_server_queue]                                       % Modules  = [Module] | dynamic
        },
        % api manager for post request
        {   hawk_server_api_manager,
            {hawk_server_api_manager, start_link, []},
            permanent,                               % Restart  = permanent | transient | temporary
            infinity,                                % Shutdown = brutal_kill | int() >= 0 | infinity
            supervisor,                              % Type     = worker | supervisor
            [hawk_server_api_manager]                                       % Modules  = [Module] | dynamic
        }
    ],

    Stat = proplists:get_value(use, hawk_server_app:get_app_env(statistic, [])),

    M =
        if
            Stat == true ->
                [Modules|% staistics
                    {   hawk_server_statistic,
                        {hawk_server_statistic,start_link,[]},
                        permanent,                               % Restart  = permanent | transient | temporary
                        2000,                                % Shutdown = brutal_kill | int() >= 0 | infinity
                        worker,                              % Type     = worker | supervisor
                        [hawk_server_statistic]                                       % Modules  = [Module] | dynamic
                    }];
            true -> Modules
        end,

    {ok,
        {_SupFlags = {one_for_one, ?MAX_RESTART, ?MAX_TIME}, M}
    }.

%% ====================================================================
%% Internal functions
%% ====================================================================

