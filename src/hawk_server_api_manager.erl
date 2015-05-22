%% @author Maximilian
%% @doc @todo Add description to hawk_server_api_manager.

%@todo модуль ведёт себя не корректно при постановке запросов в очередь,
%поэтому данный функционал пока отключен. В дальнейшем надо будет проработать этот момент
-module(hawk_server_api_manager).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-include("mac.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/0, restore_worker/1]).

%стартовое и максимальное количество воркеров
-define(MIN_WORKERS, 10).
-define(MAX_WORKERS, 500).

-define(NAME_QUEUE, api_queue).
%размер очереди на обработку
-define(MAX_QUEUE, 1000).
%процент переполнения при котором будут создаваться новые воркеры
%%для уменьшения размера очереди
-define(OVERFLOW_QUEUE, 70).

-record(state, {worker_pool=[], worker_count=0}).

init([]) ->
	process_flag(trap_exit, true),
	supervisor:start_link({local, hawk_server_api_sup}, hawk_server_api_sup, []),
	hawk_server_queue:new(?NAME_QUEUE, ?MAX_QUEUE),
    {ok, Pool} = hawk_server_api_sup:init_worker_pool(?MIN_WORKERS),
    {ok, #state{worker_pool=Pool, worker_count=?MIN_WORKERS}}.

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

handle_call(Params, From, #state{worker_count=WCount} = State) ->
%% 	?DBG(WCount),
	NewState = case get_worker(State) of
		{Pid, NewPool} ->
%% 			?DBG("new"),
			gen_server:cast(Pid, {From, Params}),
			State#state{worker_pool=NewPool};
		{Pid, NewPool, add_worker} ->
%% 			?DBG("add"),
			gen_server:cast(Pid, {From, Params}),
			State#state{worker_pool=NewPool, worker_count=WCount+1};
		{Pid, NewPool, overflow_worker} ->
%% 			?DBG("overflow"),
			gen_server:cast(Pid, {From, Params}),
			State#state{worker_pool=NewPool, worker_count=WCount+1};
		false ->
%% 			?DBG(["in q ", WCount]),
			{ok, Pid} = create_worker(),
			gen_server:cast(Pid, {From, Params}),
			State#state{worker_count=WCount+1}
			
			%@todo функционал постановки в очередь ведёт себя некорректно, процесс, сообщене
			%которого добавлено в очередь никогда не дожидается ответа, хотя его сообщение обрабатывается
			%необходимо проработать этот момент
%% 			case hawk_server_queue:insert(?NAME_QUEUE, {From, Params}) of
%% 				ok -> 
%% 					State;
%% 				false -> 
%% 					?DBG("super overflow"),
%% 					{ok, Pid} = create_worker(),
%% 					gen_server:cast(Pid, {From, Params}),
%% 					State#state{worker_count=WCount+1}
%% 			end
	end,
	
    {noreply, NewState}.

handle_cast({restore_worker, Pid}, #state{worker_pool=Pool, worker_count=WCount} = State) when is_pid(Pid) ->
%% 	?DBG(Pid),
	NewState = if
		%число воркеров больше максимального, значит очередь может существовать
		WCount > ?MAX_WORKERS ->
%% 			?DBG(WCount),
			case hawk_server_queue:shift(?NAME_QUEUE) of
				[] -> 
%% 					?DBG("terminate q empty"),
					gen_server:cast(Pid, terminate),
					State#state{worker_count=WCount-1};
				{From, Params} ->
%% 					?DBG("work with q"),
					gen_server:cast(Pid, {From, Params}),
					State
			end;
		WCount > ?MIN_WORKERS ->
%% 			?DBG("terminate min overflow"),
			gen_server:cast(Pid, terminate),
			State#state{worker_count=WCount-1};
		WCount < ?MIN_WORKERS ->
%% 			?DBG("restore"),
			State#state{worker_pool=[Pid|Pool]};
		true ->
			State
	end,
			   
	{noreply, NewState};

handle_cast(Msg, State) ->
%% 	?DBG(Msg),
    {noreply, State}.


handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%=============PUBLIC=================
restore_worker(Pid)  when is_pid(Pid) ->
  	gen_server:cast(?MODULE, {restore_worker, Pid});

restore_worker(Pid) ->
  	error_logger:info_msg("unrecognized call with data ~p ~n", [Pid]).

%=============PRIVATE=================
get_worker(#state{worker_pool=Pool, worker_count=Count} = _State) ->
%% 	?DBG(Count),
	case Pool of
		[] ->
			if
				Count =< ?MAX_WORKERS ->
%% 					?DBG("simple cr"),
					{ok, Worker} = create_worker(),
					{Worker, [], add_worker};
				true ->
					Size  = hawk_server_queue:size(?NAME_QUEUE),
					Perc = Size / ?MAX_QUEUE * 100,
					?DBG([Size, Perc]),
					if
						Perc > ?OVERFLOW_QUEUE ->
							{ok, Worker} = create_worker(),
							{Worker, [], overflow_worker};
						true ->
							false
					end
			end;
		Pool ->
			[Worker|T] = Pool,
			{Worker, T}
	end.

create_worker() ->
	hawk_server_api_sup:create_worker().