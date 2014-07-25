%% @author Максим
%% @doc @todo Add description to statistic_server.


-module(statistic_server).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/0, add_message/1, get_count_message/1, init_message/1]).



%% ====================================================================
%% Behavioural functions 
%% ====================================================================
-record(state, {count_all_message, ets_table}).

-record(message_log, {domain, cnt_mess, time}).

-record(users, {
    login,
    password,
    email,
    activation_code,
    key,
    domain
}).

%% init/1
init([]) ->
	TableId = ets:new(hosts_data, [ordered_set]),
	mongodb:singleServer(hawk_statistics),
	mongodb:connect(hawk_statistics),
	
	define_records(),
	
    {ok, #state{count_all_message=0, ets_table=TableId}}.

start_link()  ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


%% handle_call/3
handle_call({get_cnt_for_host, Host}, _From, #state{ets_table=TableId} = State) ->
	[{_, Cnt}] = ets:lookup(TableId, Host),
	Reply = {ok, Cnt},
    {reply, Reply, State};

handle_call({new_message_from_host, Host}, _From, #state{count_all_message=Cnt_mess, ets_table=TableId} = State) ->
	New_cnt = Cnt_mess + 1,
	ets:insert(TableId, {Host, New_cnt}),
	
	{{Year,Month,Day},{Hour,_,_}} = erlang:localtime(),
	Time = list_to_binary([integer_to_list(Year),integer_to_list(Month),integer_to_list(Day),integer_to_list(Hour)]),

	Mong = mongoapi:new(hawk_statistics,<<"hawk">>),
	Mong:update([{#message_log.domain, Host}, {#message_log.time, Time}], #message_log{cnt_mess = New_cnt, domain = Host, time = Time}, [upsert]),
	Reply = {ok, New_cnt},
	New_state = State#state{count_all_message=New_cnt},
	{reply, Reply, New_state};

handle_call({init_message_for_host, Host, Cnt, Key}, _From, #state{ets_table=TableId} = State) ->
	%io:format("~p key: ~p\n", [self(), Key]),
	ets:insert(TableId, {Host, Cnt, Key}),
	Reply = ok,
	{reply, Reply, State}.

%% handle_cast/2
handle_cast(_Msg, State) ->
	
    {noreply, State}.


%% handle_info/2
handle_info(_Info, State) ->
    {noreply, State}.


%% terminate/2
terminate(_Reason, _State) ->
    ok.


%% code_change/3
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% ====================================================================
%% Internal functions
%% ====================================================================
add_message(Host) ->
	gen_server:call(?MODULE, {new_message_from_host, Host}).

get_count_message(Host) ->
	gen_server:call(?MODULE, {get_cnt_for_host, Host}).

init_message(Host) ->
	%io:format("~p Init message", [self()]),
	Mong = mongoapi:new(hawk_statistics,<<"hawk">>),
	define_records(),
	B_host = list_to_binary(Host),

	{ok, User} = Mong:findOne(#users{domain = B_host}, [#users.key]),
	%io:format("~p user: ~p.\n", [self(), User]),
	case User of
		[] ->
			{ok, false};
		_ ->
			{ok, Rec} = Mong:findOne(#message_log{domain = B_host}, [#message_log.cnt_mess]),
			%io:format("~p finded rec ~p\n", [self(), Rec]),
			
			case Rec of
				[] ->
					Cnt = 0;
				_ ->
					#message_log{cnt_mess = Cnt} = Rec
			end,
			
			gen_server:cast(?MODULE, {init_message_for_host, Host, Cnt}),
			{ok, Cnt}
	end.

define_records() ->
	mongoapi:recinfo(#message_log{}, record_info(fields, message_log)),
	mongoapi:recinfo(#users{}, record_info(fields, users)).
	

