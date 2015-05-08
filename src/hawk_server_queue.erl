%% @author Maximilian
%% @doc @todo Add description to hawk_queue.


-module(hawk_server_queue).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0]).
-export([new/2, add/2, get/1, clear/1, delete/1]).

-record(state, {}).


new(Name, Length) ->
	gen_server:call(?MODULE, {new, Name, Length}).

add(Name, NData) ->
	gen_server:call(?MODULE, {add, Name, NData}).

get(Name) ->
	gen_server:call(?MODULE, {get, Name}).

clear(Name) ->
	gen_server:call(?MODULE, {clear, Name}).

delete(Name) ->
	gen_server:call(?MODULE, {delete, Name}).

init([]) ->
	dets:open_file(message_queue, [{access, read_write}, {type, set}, {auto_save, 10000}, {file, "data/message_queue"}, {ram_file, true}]),
    {ok, #state{}}.

start_link()  ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

handle_call({new, Name, Length}, _From, State) ->
    dets:insert_new(message_queue, {Name, Length, []}),
    {reply, ok, State};

handle_call({add, Name, NData}, _From, State) ->
    Reply = case dets:lookup(message_queue, Name) of
		[] ->
			false;
		[{Name, Length, Data}] ->
			CLen = length(Data),
			NewData = if 
				CLen > Length ->
					[_H|T] = Data,
					lists:reverse([NData|lists:reverse(T)]);
				true ->
					[NData|Data]
			end,
			dets:insert(message_queue, {Name, Length, NewData}),
			ok
	end,
    {reply, Reply, State};

handle_call({get, Name}, _From, State) ->
    Reply = dets:lookup(message_queue, Name),
    {reply, Reply, State};

handle_call({clear, Name}, _From, State) ->
    Reply = case dets:lookup(message_queue, Name) of
		[] ->
			false;
		[{Name, Length, _Data}] ->
			dets:insert(message_queue, {Name, Length, []})
	end,
    {reply, Reply, State};

handle_call({delete, Name}, _From, State) ->
    dets:delete(message_queue, Name),
    {reply, ok, State}.

handle_cast(_, State) ->
    {noreply, State}.


handle_info(Info, State) ->
    {noreply, State}.


terminate(Reason, State) ->
    ok.


code_change(OldVsn, State, Extra) ->
    {ok, State}.


%% ====================================================================
%% Internal functions
%% ====================================================================


