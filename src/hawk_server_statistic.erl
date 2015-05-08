%% @author Максим
%% @doc @todo Add description to hawk_server_statistic.


-module(hawk_server_statistic).
-behaviour(gen_server).
-include("env.hrl").
-include("mac.hrl").

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0, add_message/1, get_count_message/1, init_message/1]).



%% ====================================================================
%% Behavioural functions 
%% ====================================================================
-record(state, {count_all_message, ets_table}).

%% init/1
init([]) ->
	TableId = ets:new(hosts_data, [ordered_set, private]),
    {ok, #state{count_all_message=0, ets_table=TableId}}.

start_link()  ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


%% handle_call/3
handle_call({get_cnt_for_host, Host}, _From, #state{ets_table=TableId} = State) ->
	[{_, Cnt}] = ets:lookup(TableId, Host),
	Reply = {ok, Cnt},
    {reply, Reply, State};

handle_call({new_message_from_host, Host}, _From, #state{count_all_message=Cnt_mess, ets_table=TableId} = State) ->
	New_cnt = case ets:lookup(TableId, Host) of
		[] -> 
			ets:insert(TableId, {Host, 1}),
			1;
		[{Host, OldCnt}] -> 
			ets:insert(TableId, {Host, OldCnt+1}),
			(OldCnt+1)
	end,
	
	{{Year,Month,Day},{Hour,_,_}} = erlang:localtime(),
	
	if 
		Month < 10 ->
			EMonth = ["0", integer_to_list(Month)];
		true ->
			EMonth = integer_to_list(Month)
	end,

	if 
		Day < 10 ->
			EDay = ["0", integer_to_list(Day)];
		true ->
			EDay = integer_to_list(Day)
	end,

	if 
		Hour < 10 ->
			EHour = ["0", integer_to_list(Hour)];
		true ->
			EHour = integer_to_list(Hour)
	end,

	Time = list_to_binary([integer_to_list(Year), EMonth, EDay, EHour]),

	{ok, Connection} = mongo:connect (?DB_NAME),
	
	Command = {'$set', {
	    domain, hawk_server_lib:convert_to_binary({conv, Host}),
	    time, Time,
	    cnt_mess, New_cnt
	}},
	mongo:update(Connection, <<"message_log">>, {domain, Host, time, Time}, Command, true),
	mc_worker:disconnect(Connection),

	Reply = {ok, New_cnt},
	New_state = State#state{count_all_message=(Cnt_mess+1)},
	{reply, Reply, New_state};

handle_call({init_message_for_host, Host, Cnt, Key}, _From, #state{ets_table=TableId} = State) ->
	ets:insert(TableId, {Host, Cnt, Key}),
	Reply = ok,
	{reply, Reply, State}.

%% handle_cast/2
handle_cast(_Msg, State) -> {noreply, State}.

%% handle_info/2
handle_info(_Info, State) -> {noreply, State}.

%% terminate/2
terminate(_Reason, _State) -> ok.

%% code_change/3
code_change(_OldVsn, State, _Extra) -> {ok, State}.


%% ====================================================================
%% Internal functions
%% ====================================================================
add_message(Host) ->
	gen_server:call(?MODULE, {new_message_from_host, Host}).

get_count_message(Host) ->
	gen_server:call(?MODULE, {get_cnt_for_host, Host}).

init_message(Host) ->
	B_host = list_to_binary(Host),
	
	User = ?get_user_by_domain(B_host),

	case User of
		{ok, false} ->
			{ok, false};
		_ ->
			{ok, Connection} = mongo:connect (?DB_NAME),
			
			{true,{result,Res}} = mongo:command(Connection, {aggregate, <<"message_log">>, pipeline, [
					  {'$match', {domain, B_host}},
					  {'$project', {cnt_mess, true, domain, true}},
					  {'$group', {
						   '_id', <<"$domain">>, 
						   'summ', {'$sum', <<"$cnt_mess">>}
			  			}
					  }
				]}),
			
			Sum = case Res of
				[{'_id', B_host, summ, Cnt}] -> Cnt;
				[] -> 0
			end,
			
			gen_server:cast(?MODULE, {init_message_for_host, Host, Sum}),
			{ok, Sum}
	end.	

