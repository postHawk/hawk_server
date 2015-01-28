%% @author 
%% @doc @todo Add description to domain_sup.


-module(hawk_server_domain_sup).
-behaviour(supervisor).
-export([start_link/0]).
-export([init/1]).

-define(MAX_RESTART,    5).
-define(MAX_TIME,      60).
-define(DEF_PORT,    2222).
-define(MAX_MESSAGE_COUNT, 10).

%% ====================================================================
%% API functions
%% ====================================================================
-export([]).



%% ====================================================================
%% Behavioural functions 
%% ====================================================================

start_link() ->
   supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%остановка на доменном супервизоре с потомками

init([Module]) ->
    {ok,
        {_SupFlags = {simple_one_for_one, ?MAX_RESTART, ?MAX_TIME},
            [
              % TCP Client
              {   undefined,                               % Id       = internal id
                  {Module,start_link,[]},                  % StartFun = {M, F, A}
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


