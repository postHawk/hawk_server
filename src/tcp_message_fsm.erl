-module(tcp_message_fsm).
-author('saleyn@gmail.com').
 
-behaviour(gen_fsm).
 
-export([start_link/0, set_socket/4]).
-import(crypto, [hmac/2]).
 
%% gen_fsm callbacks
-export([init/1, handle_event/3,
         handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).
 
%% FSM States
-export([
    'WAIT_FOR_SOCKET'/2,
    'WAIT_FOR_DATA'/2,
	'WAIT_USER_MESSAGE'/2,
	'WAIT_LOGIN_MESSAGE'/2,
	'POST_ANSWER'/2
]).
 
-define(TIMEOUT, 120000).
-define(MAX_MESSAGE_COUNT, 10).

-include("jsonerl.hrl").

-record(message, {from, to, time, text}).
-record(to_message, {from, time, text}).
-record(register_user, {key, id}).
-record(unregister_user, {key, id}).
-record(add_domain, {key, domain, login}).
-record(del_domain, {key, domain, login}).

-record(state,{
	socket,    % client socket
	addr,       % client address
	host_name,
	count_message,
	key,
	login,
	curent_login
}).
 
%%%------------------------------------------------------------------------
%%% API
%%%------------------------------------------------------------------------
 
%%-------------------------------------------------------------------------
%% @spec (Socket) -> {ok,Pid} | ignore | {error,Error}
%% @doc To be called by the supervisor in order to start the server.
%%      If init/1 fails with Reason, the function returns {error,Reason}.
%%      If init/1 returns {stop,Reason} or ignore, the process is
%%      terminated and the function returns {error,Reason} or ignore,
%%      respectively.
%% @end
%%-------------------------------------------------------------------------
start_link() ->
    gen_fsm:start_link(?MODULE, [], []).
 
set_socket(Pid, Socket, Data, Host) when is_pid(Pid), is_port(Socket) ->
    gen_fsm:send_event(Pid, {socket_ready, Socket, Host}),
	gen_fsm:send_event(Pid, {data, Data}).
 
%%%------------------------------------------------------------------------
%%% Callback functions from gen_server
%%%------------------------------------------------------------------------
 
%%-------------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, StateName, StateData}          |
%%          {ok, StateName, StateData, Timeout} |
%%          ignore                              |
%%          {stop, StopReason}
%% @private
%%-------------------------------------------------------------------------
init([]) ->
	crypto:start(),
    process_flag(trap_exit, true),
    {ok, 'WAIT_FOR_SOCKET', #state{}}.
 
%%-------------------------------------------------------------------------
%% Func: StateName/2
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% @private
%%-------------------------------------------------------------------------
'WAIT_FOR_SOCKET'({socket_ready, Socket, H_name}, State) when is_port(Socket) ->
    % Now we own the socket
	%io:format("~p WAIT_FOR_SOCKET\n", [self()]),
    inet:setopts(Socket, [{active, once}, binary]),
	
    {next_state, 'WAIT_FOR_DATA', State#state{socket=Socket, host_name=H_name, count_message=0}, ?TIMEOUT};

'WAIT_FOR_SOCKET'(Other, State) ->
    error_logger:error_msg("State: 'WAIT_FOR_SOCKET'. Unexpected message: ~p\n", [Other]),
    %% Allow to receive async messages
    {next_state, 'WAIT_FOR_SOCKET', State}.
 
%% Notification event coming from client
'WAIT_FOR_DATA'({data, Data}, #state{socket=S} = State) ->
	%io:format("~p WAIT_FOR_DATA\n", [self()]),
	%io:format("~p data: ~p\n", [self(), Data]),
	case tcp_lib:is_post_req(Data) of
        true -> 
			%io:format("~p is POST\n", [self()]),
			inet:setopts(S, [{active, false}, binary]),
			?MODULE:'POST_ANSWER'({data, Data}, State);
        false ->  
			case re:run(Data, "Sec-WebSocket-Key:[\s]{0,1}(.*)\r\n",[global,{capture,[1],list}]) of
		        {match, Res} -> 
		        	%io:format("~p is Handshake\n", [self()]),
					{ok, B_all_answ} = get_awsw_key(Res);
		        nomatch ->  
					B_all_answ = Data
		    end,
			ok = gen_tcp:send(S, B_all_answ),
			{next_state, 'WAIT_LOGIN_MESSAGE', State#state{count_message=0}}
    end;
		 
'WAIT_FOR_DATA'(timeout, State) ->
    error_logger:error_msg("~p Client connection timeout - closing.\n", [self()]),
    {stop, normal, State};
 
'WAIT_FOR_DATA'(_Data, State) ->
    %io:format("~p Ignoring data: ~p\n", [self(), Data]),
    {next_state, 'WAIT_FOR_DATA', State, ?TIMEOUT}.

'WAIT_LOGIN_MESSAGE'({data, Bin}, #state{socket=S, host_name=H_name} = State) ->
 	%io:format("~p WAIT_LOGIN_MESSAGE\n", [self()]),
	{ok, Data} =  handle_data(Bin),
	%io:format("~p User login: ~p\n", [self(), Data]),
	%io:format("~p User addr: ~p\n", [self(), Address]),
	%io:format("~p User host: ~p\n", [self(), H_name]),
 	
	case check_login_format(Data) of
		true ->
		 	StrLogin = list_to_binary([H_name, "_", Data]),
			Login = tcp_lib:get_login([H_name, "_", Data]),

			case Login of
				false ->
					tcp_listener:delete_user_pid(self(), Login),
		    		%io:format("~p Client ~p disconnected.\n", [self(), OldLogin]),
					{stop, normal, State};
				_ ->
					%io:format("~p User full login: ~p\n", [self(), Login]),
					%io:format("~p User exists: ~p\n", [self(), tcp_listener:is_user_exists(binary_to_atom(Data, utf8))]),
					%io:format("~p Domain ~p exists: ~p\n", [self(), H_name, tcp_listener:is_user_domain_exists(binary_to_atom(Data, utf8), H_name)]),
					
					case tcp_listener:is_user_domain_exists(binary_to_atom(Data, utf8), list_to_binary(lists:flatten(H_name))) of
						{ok,true} ->
							case tcp_listener:is_user_exists(binary_to_atom(Data, utf8)) of
								{ok,true} ->
									case statistic_server:init_message(H_name) of
										{ok, false} ->
											{ok, Frame} = mask(<<"invalid_key">>),
									  	 	ok = gen_tcp:send(S, Frame),
											{next_state, 'WAIT_LOGIN_MESSAGE', State};

										{ok, Cnt} ->
											NewLogin = tcp_lib:get_uniq_user_login(StrLogin),
											register(NewLogin, self()),

											tcp_listener:add_user_pid(self(), Login),

											{ok, Frame} = mask(<<"ok">>),
											ok = gen_tcp:send(S, Frame),
											
											{next_state, 'WAIT_USER_MESSAGE', State#state{count_message=Cnt, login=Login, curent_login=NewLogin}}
									end;
								{ok,false} ->
									{ok, Frame} = mask(<<"user_not_register">>),
							  	 	ok = gen_tcp:send(S, Frame),
									{next_state, 'WAIT_LOGIN_MESSAGE', State}
							end;
						{ok,false} ->
							{ok, Frame} = mask(<<"domain_not_register">>),
					  	 	ok = gen_tcp:send(S, Frame),
					  	 	gen_tcp:close(S),

							{stop, normal, State}
					end
			end;
		false ->
			case erlang:port_info(S) of
				undefined ->
					true;
				_ -> 
				{ok, Frame} = mask(<<"invalid_login_format">>),
	  	 		ok = gen_tcp:send(S, Frame)
			end,
			{next_state, 'WAIT_LOGIN_MESSAGE', State}
	end.


'WAIT_USER_MESSAGE'({data, Bin}, #state{socket=S, host_name=H_name, curent_login=CurentLogin, count_message=OldCnt} = State) ->
	{ok, Data} =  handle_data(Bin),
	%io:format("~p data: ~p\n", [self(), Data]),

	J_data = get_json_data(message, Data),

	%io:format("~p json data: ~p\n", [self(), J_data]),
	
	case J_data of
		{message, From, To, Time, Text} -> 
			%%io:format("~p User message: ~p\n", [self(), J_data]);
			To_data = #message{from=From, to=To, time=Time, text=Text},
			
			Login = tcp_lib:get_login([H_name, "_", To]),

			%запрещаем отправку сообщение самому себе
			if 
				Login =/= CurentLogin->
					case Login of
						false ->
							{ok, Frame} = mask(<<"invalid_login_data">>),
							Cnt = OldCnt,
							ok = gen_tcp:send(S, Frame);
						_ ->
							case tcp_listener:get_user_pids(To) of
							 	[] ->
							 		{ok, Frame} = mask(<<"user_not_exists">>),
							 		Cnt = OldCnt,
							 		ok = gen_tcp:send(S, Frame);
							 	Pids ->
							 		lists:foreach(fun(Pid)->
							 				%можно было бы поставить гварда, но нужно вычищать мёртвые процессы
							 				%поэтому сделаем case
							 				case is_pid(Pid) of
							 					true ->
							 						%io:format("~p send message ~p to: ~p\n", [self(), To_data, Pid]),
							 						Pid ! {new_message, To_data},
													{ok, Frame} = mask(<<"ok">>),
													ok = gen_tcp:send(S, Frame);
												false ->
													%io:format("~p remove dead proc: ~p\n", [self(), Pid]),
													tcp_listener:delete_user_pid(Pid, Login)
							 				end
							 		end, Pids),
							 		{ok, Cnt} = statistic_server:add_message(H_name)
							 end
					end;
				true ->
					{ok, Frame} = mask(<<"send_message_yourself">>),
					Cnt = OldCnt,
					ok = gen_tcp:send(S, Frame)
			end;
		_ when is_port(S) -> 
			Cnt = OldCnt,
			case erlang:port_info(S) of
				undefined ->
					true;
				_ -> 
				{ok, Frame} = mask(<<"invalid_format_data">>),
				ok = gen_tcp:send(S, Frame)
			end
	end,
		
	{next_state, 'WAIT_USER_MESSAGE', State#state{count_message=Cnt}};

'WAIT_USER_MESSAGE'({new_message, Bin}, #state{socket=S} = State) ->
	%io:format("~p Message in handle: ~p\n", [self(), Bin]),
	#message{from=From, time=Time, text=Text} = Bin,
	
	To_data = #to_message{from=From, time=Time, text=Text},
	
	{ok, Frame} = mask(list_to_binary(?record_to_json(to_message, To_data))),
	ok = gen_tcp:send(S, Frame),
	
	{next_state, 'WAIT_USER_MESSAGE', State}.

'POST_ANSWER'({data, Data}, #state{socket=S} = State) ->
	%io:format("~p POST_ANSWER\n", [self()]),
	
	case re:run(Data, "Transport\: sokets", [global]) of
		nomatch ->  
			%используется курл
			{ok, POST_data} = gen_tcp:recv(S, 0),
			Post = tcp_lib:get_json_from_post(POST_data);
		{match, _} ->
			%используется соединение на сокетах
			[_Headers, Msg] = re:split(Data, "\r\n\r\n"),
			Post = tcp_lib:split_json_by_part(binary_to_list(Msg))
	end,
       
	%io:format("~p post messge ~p\n", [self(), POST_data]),	

	case Post of
		false ->
			Res = <<"unknow data type">>;
		{ok, Qtype, StrJSON} ->
			%io:format("~p post type ~p\n", [self(), tcp_lib:convert_to_atom(Qtype)]),
			JSON = 
				try get_json_data(tcp_lib:convert_to_atom(Qtype), list_to_binary(StrJSON)) of
					J -> J
				catch  _A:_B -> 
					%io:format("~p error ~p:~p  \n", [self(), A, B]),
					false
				end,

			%io:format("~p res json ~p\n", [self(), JSON]),

			Res = 
				try 
					action_on_user(JSON) of
					R -> R
				catch  _C:_D -> 
					%io:format("~p error ~p:~p  \n", [self(), C, D]),
					<<"invalid_format_data">>
				end
	end,

	Answer = [
%		"Content-type: text/html\r\n",
  %  	"Cache-Control: no-store, no-cache, must-revalidate, post-check=0, pre-check=0\r\n",
  %  	"Pragma: no-cache\r\n",
  %  	"Connection: Keep-Alive\r\n",
  %  	"Content-Length: ", byte_size(Res), 
  		"\r\n",
    	Res
    ],

	%io:format("~p send res ~p sock_r \n", [self(), Res]),
	gen_tcp:send(S, list_to_binary(Answer)),
	gen_tcp:close(S),

	{stop, normal, State}.
 
action_on_user({register_user, Key, Id}) ->
	case check_login_format(Id) of
		true ->
			atom_to_list(tcp_listener:rergister_user(Key, Id));
		false ->
			"invalid_login_format"
	end;

action_on_user({unregister_user, Key, Id}) ->
	case check_login_format(Id) of
		true ->
			atom_to_list(tcp_listener:unregister_user(Key, Id));
		false ->
			"invalid_login_format"
	end;

action_on_user({add_domain, Key, Domain, Login}) ->
	atom_to_list(tcp_listener:add_domain(Key, Domain, Login));

action_on_user({del_domain, Key, Domain, Login}) ->
	atom_to_list(tcp_listener:del_domain(Key, Domain, Login)).

check_login_format(Data) ->
	%io:format("~p check_login_format ~p result ~p \n", [self(), Data, re:run(Data, "^[a-zA-Z\d]{3,64}$")]),
	case re:run(Data, "^[a-zA-Z0-9]{3,64}$") of
        {match, _} -> 
        	true;
        nomatch ->
        	false
      end.

%%-------------------------------------------------------------------------
%% Func: handle_event/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% @private
%%-------------------------------------------------------------------------
handle_event(Event, StateName, StateData) ->
    {stop, {StateName, undefined_event, Event}, StateData}.
 
%%-------------------------------------------------------------------------
%% Func: handle_sync_event/4
%% Returns: {next_state, NextStateName, NextStateData}            |
%%          {next_state, NextStateName, NextStateData, Timeout}   |
%%          {reply, Reply, NextStateName, NextStateData}          |
%%          {reply, Reply, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}                          |
%%          {stop, Reason, Reply, NewStateData}
%% @private
%%-------------------------------------------------------------------------
handle_sync_event(Event, _From, StateName, StateData) ->
    {stop, {StateName, undefined_event, Event}, StateData}.
 
%%-------------------------------------------------------------------------
%% Func: handle_info/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% @private
%%-------------------------------------------------------------------------
handle_info({tcp, Socket, Bin}, StateName, #state{socket=Socket} = StateData) ->
    % Flow control: enable forwarding of next TCP message
	
    inet:setopts(Socket, [{active, once}]),
	%io:format("~p is GET\n", [self()]),
   	?MODULE:StateName({data, Bin}, StateData);
    
 
handle_info({tcp_closed, Socket}, _StateName,
            #state{socket=Socket, addr=Addr, login=Login} = StateData) ->
	tcp_listener:delete_user_pid(self(), Login),
    error_logger:info_msg("~p Client ~p disconnected.\n", [self(), Addr]),
    {stop, normal, StateData};
 
handle_info(Data, StateName, StateData) ->
	%io:format("~p Message from proc: ~p ~p ~p\n", [self(), Data, StateName, StateData]),
	 ?MODULE:StateName(Data, StateData).
 
%%-------------------------------------------------------------------------
%% Func: terminate/3
%% Purpose: Shutdown the fsm
%% Returns: any
%% @private
%%-------------------------------------------------------------------------
terminate(_Reason, _StateName, #state{socket=Socket}) ->
    (catch gen_tcp:close(Socket)),
    ok.
 
%%-------------------------------------------------------------------------
%% Func: code_change/4
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState, NewStateData}
%% @private
%%-------------------------------------------------------------------------
code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.

get_awsw_key(Res) ->
	[H|_] = Res,
	[Key|_] = H,
	Key1 = [Key, "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"],	
	<<Mac/binary>> = crypto:hash(sha, list_to_binary(Key1)),

	B_prot = <<"HTTP/1.1 101 Web Socket Protocol Handshake\r\n">>,
	B_upg = <<"Upgrade: WebSocket\r\n">>,
	B_conn = <<"Connection: Upgrade\r\n">>, 
	Answ_key = [<<"Sec-WebSocket-Accept: ">>, base64:encode(Mac), <<"\r\n\r\n">>],
	B_key = list_to_binary(Answ_key),

	B_all_answ = <<B_prot/binary, B_upg/binary, B_conn/binary, B_key/binary>>,
	{ok, B_all_answ}.

unmask(Payload, Masking) ->
    unmask(Payload, Masking, <<>>).

unmask(Payload, Masking = <<MA:8, MB:8, MC:8, MD:8>>, Acc) ->
    % io:format("acc ~p rest ~w~n", [Acc, Payload]),
    case size(Payload) of
        0 -> Acc;
        1 ->
            <<A:8>> = Payload,
            <<Acc/binary, (MA bxor A)>>;
        2 ->
            <<A:8, B:8>> = Payload,
            <<Acc/binary, (MA bxor A), (MB bxor B)>>;
        3 ->
            <<A:8, B:8, C:8>> = Payload,
            <<Acc/binary, (MA bxor A), (MB bxor B), (MC bxor C)>>;
        _Other ->
            <<A:8, B:8, C:8, D:8, Rest/binary>> = Payload,
            Acc1 = <<Acc/binary, (MA bxor A), (MB bxor B), (MC bxor C), (MD bxor D)>>,
            unmask(Rest, Masking, Acc1)
    end.

mask(Data) ->
	Len = size(Data),
	if 	
		Len =:= 126 ->
			Frame = <<1:1, 0:3, 1:4, 0:1, 126:7, Len:16, Data/binary>>;
		Len > 126 ->
			Frame = <<1:1, 0:3, 1:4, 0:1, 127:7, Len:64, Data/binary>>;
		true -> 
			Frame = <<1:1, 0:3, 1:4, 0:1, Len:7, Data/binary>>
	end,
	{ok, Frame}.

handle_data(Data) ->
    <<_Fin:1, _Rsv:3, _Opcode:4, _Mask:1, Len:7, Rest/binary>> = Data,
    
	if 	
		Len =:= 126 ->
			<<Dop_len:16/unsigned-integer, Masking:4/binary, Payload:Dop_len/binary, _Next/binary>> = Rest;
		Len > 126 ->
			<<Dop_len:64/unsigned-integer, Masking:4/binary, Payload:Dop_len/binary, _Next/binary>> = Rest;
		true -> 
			_Dop_len = 0,
			<<Masking:4/binary, Payload:Len/binary, _Next/binary>> = Rest
	end,
    
    %io:format("fin ~p op ~p len ~p sup_len ~p masked ~p key ~w data ~w~n", [Fin, Opcode, Len, Dop_len, Mask, Masking, Payload]),
    Line = unmask(Payload, Masking),
    {ok, Line}.

get_json_data(message, Data) ->
	try ?json_to_record(message, Data) of
		R -> R
	catch  _:_ -> 
		false
	end;

get_json_data(register_user, Data) ->
	try ?json_to_record(register_user, Data) of
		R -> R
	catch  _:_ -> 
		false
	end;

get_json_data(unregister_user, Data) ->
	try ?json_to_record(unregister_user, Data) of
		R -> R
	catch  _:_ -> 
		false
	end;

get_json_data(add_domain, Data) ->
	try ?json_to_record(add_domain, Data) of
		R -> R
	catch  _:_ -> 
		false
	end;

get_json_data(del_domain, Data) ->
	try ?json_to_record(del_domain, Data) of
		R -> R
	catch  _:_ -> 
		false
	end.	