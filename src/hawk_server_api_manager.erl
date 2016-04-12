%% @author Barulin Maxim <mbarulin@gmail.com>
%% @copyright 2016 Barulin Maxim
%% @version 0.0.3
%% @title hawk_server_api_manager
%% @doc Модуль, управляющий пулом рабочих процессов и запросами к ним
%@todo модуль ведёт себя не корректно при постановке запросов в очередь,
%@todo поэтому данный функционал пока отключен. В дальнейшем надо будет проработать этот момент

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

%%название очереди для апи-запросов
-define(NAME_QUEUE, api_queue).
%размер очереди на обработку
-define(MAX_QUEUE, 1000).
%процент переполнения при котором будут создаваться новые воркеры
%%для уменьшения размера очереди
-define(OVERFLOW_QUEUE, 70).

-record(state, {worker_pool=[], worker_count=0}).

%% @doc Инициализация модуля. Создание пула процессов-обработчиков.
init([]) ->
	process_flag(trap_exit, true),
	supervisor:start_link({local, hawk_server_api_sup}, hawk_server_api_sup, []),
	hawk_server_queue:new(?NAME_QUEUE, ?MAX_QUEUE),
    {ok, Pool} = hawk_server_api_sup:init_worker_pool(?MIN_WORKERS),
    {ok, #state{worker_pool=Pool, worker_count=?MIN_WORKERS}}.

%% @doc Запуск модуля
start_link() ->
	gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

-spec handle_call(Params :: any(), From :: {pid(), reference()}, State :: #state{}) -> {noreply, NewState :: #state{}}.
%% @doc Обработка одного запроса к апи.
handle_call(Params, From, #state{worker_count=WCount} = State) ->
	%получаем процесс для обработки запроса
	%и определяемся с состоянием пула
	NewState =
		case get_worker(State) of
			%получили процесс из списка запущенных
			{Pid, NewPool} ->
				gen_server:cast(Pid, {From, Params}),
				State#state{worker_pool=NewPool};
			%добавили еще один процесс, пока в рамках максимального количества
			{Pid, NewPool, add_worker} ->
				gen_server:cast(Pid, {From, Params}),
				State#state{worker_pool=NewPool, worker_count=WCount+1};
			%превышен максимальный процент переполнения очереди,
			%создан новый процесс для обработки сообщений
			{Pid, NewPool, overflow_worker} ->
				gen_server:cast(Pid, {From, Params}),
				State#state{worker_pool=NewPool, worker_count=WCount+1};
			%процент переполнения пока не достигнут
			%ставим сообщение в очередь
			false ->
				{ok, Pid} = create_worker(),
				gen_server:cast(Pid, {From, Params}),
				State#state{worker_count=WCount+1}

	           %@todo функционал постановки в очередь ведёт себя некорректно, процесс, сообщене
	           %@todo которого добавлено в очередь никогда не дожидается ответа, хотя его сообщение обрабатывается
	           %@todo необходимо перейти с таблицы на очередь сообщений процесса
	%% 			case hawk_server_queue:insert(?NAME_QUEUE, {From, Params}) of
	%% 				ok ->
	%% 					State;
	%% 				false ->
	%% 					{ok, Pid} = create_worker(),
	%% 					gen_server:cast(Pid, {From, Params}),
	%% 					State#state{worker_count=WCount+1}
	%% 			end
		end,
	
    {noreply, NewState}.

-spec handle_cast({atom(), pid()}, State :: #state{}) -> {noreply, NewState :: #state{}}.
%% @doc возвращает отработавший процесс в режим ожидания
handle_cast({restore_worker, Pid}, #state{worker_pool=Pool, worker_count=WCount} = State) when is_pid(Pid) ->
	NewState =
		if
			%число воркеров больше максимального, значит очередь может существовать
			WCount > ?MAX_WORKERS -> add_to_queue(Pid, State);
		   %число процессов больше максимального, закрываем
			WCount > ?MIN_WORKERS ->
				gen_server:cast(Pid, terminate),
				State#state{worker_count=WCount-1};
			%число процессов в заданном диапазоне,
			%добавлем процесс к ожидающим
			WCount < ?MIN_WORKERS -> State#state{worker_pool=[Pid|Pool]};
			true -> State
		end,

	{noreply, NewState};

handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

-spec restore_worker(pid()) -> ok.
%% @doc возвращает отработавший процесс в режим ожидания
restore_worker(Pid)  when is_pid(Pid) -> gen_server:cast(?MODULE, {restore_worker, Pid});
restore_worker(Pid) -> error_logger:info_msg("unrecognized call with data ~p ~n", [Pid]).

-spec add_to_queue(pid(), State :: #state{}) -> ok.
%% @doc ставит сообщение в очередь
%% @private
add_to_queue(Pid, #state{worker_count=WCount} = State) ->
	case hawk_server_queue:shift(?NAME_QUEUE) of
		[] ->
			gen_server:cast(Pid, terminate),
			State#state{worker_count=WCount-1};
		{From, Params} ->
			gen_server:cast(Pid, {From, Params}),
			State
	end.

-spec get_worker(State :: #state{}) -> {pid(), list()} | {pid(), list(), add_worker} | {pid(), list(), overflow_worker} | false.
%% @doc возвращает процесс для обработки запроса
%% @private
get_worker(#state{worker_pool=Pool, worker_count=Count} = _State) ->
	case Pool of
		[] -> get_overflow_worker(Count);
		Pool ->
			[Worker|T] = Pool,
			{Worker, T}
	end.

-spec get_overflow_worker(integer()) -> {pid(), [], atom()} | false.
%% @doc возвращает процесс на основании переполнения очереди
%% @private
get_overflow_worker(Count) ->
	if
		%если число процессов не достигло максимального
		%просто создаём еще один
		Count =< ?MAX_WORKERS ->
			{ok, Worker} = create_worker(),
			{Worker, [], add_worker};
		true ->
			%считаем процент переполнения очереди
			Size  = hawk_server_queue:size(?NAME_QUEUE),
			Perc = Size / ?MAX_QUEUE * 100,
			%и создаём новый процесс или ставим сообщение в очередь
			if
				Perc > ?OVERFLOW_QUEUE ->
					{ok, Worker} = create_worker(),
					{Worker, [], overflow_worker};
				true ->
					false
			end
	end.

%% @doc создаёт дополнительный процесс-обработчик
%% @private
create_worker() -> hawk_server_api_sup:create_worker().
