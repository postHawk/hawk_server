%% @author Barulin Maxim <mbarulin@gmail.com>
%% @copyright 2016 Barulin Maxim
%% @version 0.0.3
%% @title hawk_server_api_sup
%% @doc Супервизор пула процессов, обрабатывающих api запросы


-module(hawk_server_api_sup).
-behaviour(supervisor).
-include("env.hrl").
-include("mac.hrl").
-export([init/1]).

-export([start_link/0]).
-export([init_worker_pool/1, create_worker/0]).

start_link() ->
   supervisor:start_link({local, ?MODULE}, ?MODULE, []).

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


-spec init_worker_pool(integer()) -> ok.
-spec init_worker_pool(integer(), list()) -> {ok, list()}.
%% @doc создаёт пулл процессов в заданном количестве
init_worker_pool(Count) -> init_worker_pool(Count, []).
init_worker_pool(0, Childs) -> {ok, Childs};
init_worker_pool(Count, Childs) ->
	{ok, Pid} = create_worker(),
	init_worker_pool(Count-1, [Pid|Childs]).

%% @doc даёт команду супервизору на запуск процесса-обработчика
create_worker() -> supervisor:start_child(?MODULE, []).
