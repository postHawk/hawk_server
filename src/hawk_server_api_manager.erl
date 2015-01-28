%% @author Maximilian
%% @doc @todo Add description to hawk_server_api_manager.


-module(hawk_server_api_manager).
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
	process_flag(trap_exit, true),
	supervisor:start_link({local, hawk_server_api_sup}, hawk_server_api_sup, []),
    
    {ok, #state{worker_pool=[]}}.

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

handle_call(Params, From, State) ->
    {ok, Pid} = hawk_server_api_sup:get_worker(),
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



