%% @author Maximilian
%% @doc @todo Add description to api_manager.


-module(api_manager).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/0]).

-define(MAX_WORKER, 10).
-define(MIN_WORKER, 5).

-record(state, {worker_pool}).

init([]) ->

	supervisor:start_link({global, api_sup}, api_sup, []),
	
	%информация о зарегистрированных пользователях
    ets:new(reg_users_data, [ordered_set, public, named_table]),
    %информация о процессах пользователя
    ets:new(users_pids, [ordered_set, public, named_table]),
    %информация о регистрационыых заспиях (на сайте сервиса из монги)
    ets:new(main_user_data, [ordered_set, public, named_table]),
    %принадлежность пользователя к  группе
    ets:new(groups_to_user, [ordered_set, public, named_table]),
    
    {ok, #state{worker_pool=[]}}.

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

handle_call(Params, From, State) ->
    {ok, Pid} = api_sup:get_worker(),
	gen_server:cast(Pid, {From, Params}),

    {noreply, State}.


handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.



