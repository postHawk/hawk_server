-module(hawk_server_chat_worker_old).
-author('mbarulin@gmail.com').
 
-behaviour(gen_fsm).
 
-export([start_link/0, set_socket/4]).
-import(crypto, [hmac/2]).
-include("mac.hrl").
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
 
-include("../deps/jsonerl/src/jsonerl.hrl").

-record(message, {from, to, time, text}).

-record(register_user, {key, id}).
-record(unregister_user, {key, id}).

-record(add_domain, {key, domain, login}).
-record(del_domain, {key, domain, login}).

-record(add_in_groups, {key, id, groups}).
-record(remove_from_groups, {key, id, groups}).
-record(send_group_message, {key, text, groups, time, from}).
-record(get_by_group, {key, groups}).
-record(users_in_group, {group, user, online}).


-record(state,{
	socket,    % client socket
	addr,       % client address
	host_name,
	count_message,
	key,
	login,
	curent_login,
	register_login,
	transport
}).
 
%%%------------------------------------------------------------------------
%%% API
%%%------------------------------------------------------------------------
 
get_data_from_worker(Params) ->
	gen_server:call(hawk_server_api_manager, Params).


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
%% 	global:unregister_name(hawk_server_lib:pid_2_name(self())),
%% 	unregister(hawk_server_lib:pid_2_name(self())),
    process_flag(trap_exit, true),
    {ok, 'WAIT_FOR_SOCKET', #state{}}.
 
%%-------------------------------------------------------------------------
%% Func: StateName/2
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% @private
%%-------------------------------------------------------------------------
'WAIT_FOR_SOCKET'({socket_ready, Socket, H_name, Transport}, State) when is_port(Socket) ->
    % Now we own the socket
    inet:setopts(Socket, [{active, once}, binary]),
	
    {next_state, 'WAIT_FOR_DATA', State#state{
		  socket=Socket, 
		  host_name=H_name, 
		  count_message=0, 
		  transport=Transport
	 	}, 
	 ?TIMEOUT};

'WAIT_FOR_SOCKET'(Other, State) ->
    error_logger:error_msg("State: 'WAIT_FOR_SOCKET'. Unexpected message: ~p\n", [Other]),
    %% Allow to receive async messages
    {next_state, 'WAIT_FOR_SOCKET', State}.

 %===============================================
%% Notification event coming from client
'WAIT_FOR_DATA'({data, Data}, State) ->
	handle_req_by_type(hawk_server_lib:is_post_req(Data), Data, State);
		 
'WAIT_FOR_DATA'(timeout, State) ->
    error_logger:error_msg("~p Client connection timeout - closing.\n", [self()]),
    {stop, normal, State};
 
'WAIT_FOR_DATA'(_Data, State) ->
    {next_state, 'WAIT_FOR_DATA', State, ?TIMEOUT}.

 handle_req_by_type(post, Data, #state{socket=S} = State) ->
 	inet:setopts(S, [{active, false}, binary]),
	?MODULE:'POST_ANSWER'({data, Data}, State);

 handle_req_by_type(get, Data, #state{socket=S, transport=Transport} = State) ->
 	
	{ok, {http_request,_Method,{abs_path, _URL},_}, H} = erlang:decode_packet(http, Data, []),
	[Headers, {body, _Body}] = hawk_server_lib:parse_header(H),
	
	Key = hawk_server_lib:get_header(<<"Sec-WebSocket-Key">>, Headers),
	
	case Key of
        undefined -> B_all_answ = ?ERROR_INVALID_HANDSHAKE;
		_ -> {ok, B_all_answ} = get_awsw_key(Key)
    end,

	hawk_server_lib:send_message({ok, B_all_answ}, S, Transport),
	{next_state, 'WAIT_LOGIN_MESSAGE', State#state{count_message=0}}.
 
 %===============================================
'WAIT_LOGIN_MESSAGE'({data, Bin}, State) ->
	{ok, Data} =  handle_data(Bin),
	handle_login_format(check_login_format(Data), Data, State).

handle_login_format(true, Data, #state{host_name=H_name} = State) ->
	StrLogin = list_to_binary([H_name, "_", Data]),

	gproc:reg({p,l,user_login}, StrLogin),
	
	User_id = binary_to_atom(Data, utf8),
	Ex_domain = get_data_from_worker({check_user_domain, User_id, list_to_binary(lists:flatten(H_name))}),
	Ex_user = get_data_from_worker({check_user, User_id}),
	Init_m = hawk_server_statistic:init_message(H_name),
	
	handle_login_main_data(Ex_domain, Ex_user, Init_m, StrLogin, Data, State);

handle_login_format(false, _Data, #state{socket=S, transport=Transport} = State) ->
	case erlang:port_info(S) of
		undefined ->
			true;
		_ -> 
			hawk_server_lib:send_message(mask(?ERROR_INVALID_LOGIN_FORMAT), S, Transport)
	end,
	{next_state, 'WAIT_LOGIN_MESSAGE', State}.

handle_login_main_data({ok,false, Reason}, _, _, _, _, #state{socket=S, transport=Transport} = State) ->
	Msg = 
		case Reason of
			no_id -> ?ERROR_USER_NOT_REGISTER;
			no_login -> ?ERROR_INVALID_KEY;
			no_domain -> ?ERROR_DOMAIN_NOT_REGISTER
		end,
	
	hawk_server_lib:send_message(mask(Msg), S, Transport),
  	Transport:close(S),
	{stop, normal, State};

handle_login_main_data({ok,true}, {ok,false}, _, _, _, #state{socket=S, transport=Transport} = State) ->
  	 	hawk_server_lib:send_message(mask(?ERROR_USER_NOT_REGISTER), S, Transport),
		{next_state, 'WAIT_LOGIN_MESSAGE', State};

handle_login_main_data({ok,true}, {ok,true}, {ok,false}, _, _, #state{socket=S, transport=Transport} = State) ->
  	 	hawk_server_lib:send_message(mask(?ERROR_INVALID_KEY), S, Transport),
		{next_state, 'WAIT_LOGIN_MESSAGE', State};

handle_login_main_data({ok,true}, {ok,true}, {ok, Cnt}, StrLogin, Register_login, #state{socket=S, transport=Transport} = State) ->
 	NewLogin = hawk_server_lib:get_uniq_user_login(StrLogin),
%% 	global:register_name(NewLogin, self()),

	hawk_server_lib:send_message(mask(?OK), S, Transport),
	
	{next_state, 'WAIT_USER_MESSAGE', State#state{count_message=Cnt, curent_login=NewLogin, register_login=Register_login}}.

 %===============================================
'WAIT_USER_MESSAGE'({data, Bin}, #state{socket=S, login=Login, transport=Transport} = State) ->
	{ok, Data} =  handle_data(Bin),
	J_data = get_json_data(message, Data),

%% 	?DBG(J_data),
	case J_data of
		false ->
			disconect_user(Login),
			Transport:close(S),
			{stop, normal, State};
		_ ->
			Cnt = handle_json_message(J_data, State),
			{next_state, 'WAIT_USER_MESSAGE', State#state{count_message=Cnt}}
	end;

'WAIT_USER_MESSAGE'({new_message, Bin}, #state{socket=S, transport=Transport} = State) ->
	{ok, Frame} = mask(list_to_binary(?record_to_json(message, Bin))),
	hawk_server_lib:send_message({ok, Frame}, S, Transport),
	
	{next_state, 'WAIT_USER_MESSAGE', State};

'WAIT_USER_MESSAGE'({'EXIT', _Pid, _Reason}, State) ->
	{next_state, 'WAIT_USER_MESSAGE', State}.

handle_json_message({message, From, {{<<"user">>, To}}, Time, Text}, #state{socket=S, host_name=H_name, curent_login=CurentLogin, count_message=OldCnt, transport=Transport} = State) ->
	To_data = #message{from=From, to=To, time=Time, text=Text},
	Login = hawk_server_lib:get_login([H_name, "_", To]),

	if 
		Login =/= CurentLogin ->
			case Login of
				false ->
					Cnt = OldCnt,
					hawk_server_lib:send_message(mask(?ERROR_INVALID_LOGIN_DATA), S, Transport);
				_ ->
					Cnt = handle_user_message(on_output, get_data_from_worker({get_pids, To}), OldCnt, To_data, Login, State)
			end;
		true ->
			Cnt = OldCnt,
			hawk_server_lib:send_message(mask(?ERROR_SEND_MESSAGE_YOURSELF), S, Transport)
	end,
	Cnt;

handle_json_message({message, From, {{<<"group">>, To}}, Time, Text}, #state{socket=S, count_message=Cnt, register_login=Login, transport=Transport} = State) when is_list(To) ->
    case ets:lookup(reg_users_data, Login) of
        [] -> 
        	Reply = ?ERROR_USER_NOT_REGISTER;
        [{_, MLogin}] ->
            case ets:lookup(main_user_data, MLogin) of
                [] -> 
                	Reply = ?ERROR_GENERAL_ERROR;
                [{_, _, Key}] ->
                    action_on_user({send_group_message, Key, Text, To, Time, From}, State),
                    Reply = ?OK
            end
    end,
    hawk_server_lib:send_message(mask(Reply), S, Transport),
	Cnt+1;

handle_json_message({message, From, {{<<"user">>, ToUser},{<<"group">>, ToGrp}}, Time, Text},  #state{count_message=Cnt} = State) when is_list(ToGrp) ->
    handle_json_message({message, From, {{<<"user">>, ToUser}}, Time, Text}, State) ,
    handle_json_message({message, From, {{<<"group">>, ToGrp}}, Time, Text}, State),
	Cnt+1;

handle_json_message({message, _From, _To, _Time, _Text}, #state{socket=S, count_message=Cnt, transport=Transport}) ->
	hawk_server_lib:send_message(mask(?ERROR_INVALID_FORMAT_DATA), S, Transport),
	Cnt;

handle_json_message(false, #state{socket=S, count_message=Cnt, transport=Transport}) ->
	hawk_server_lib:send_message(mask(?ERROR_INVALID_FORMAT_DATA), S, Transport),
	Transport:close(S),
	Cnt.

 %===============================================

handle_user_message(Output, [], Cnt, _To_data, _Login, #state{socket=S, transport=Transport} = _State) ->
	case Output of
		on_output ->
			hawk_server_lib:send_message(mask(?ERROR_USER_NOT_ONLINE), S, Transport);
		_ ->
			true
	end,
	
	Cnt;

handle_user_message(Output, Pids, _Cnt, To_data, Login, #state{host_name=H_name, socket=S, transport=Transport} = _State) ->
	lists:foreach(fun(Pid)->
		if is_pid(Pid) ->
				case is_process_alive(Pid) of
					true ->
						Pid ! {new_message, To_data},
						Reply = ?OK;
					false ->
						Reply = ?ERROR_USER_NOT_ONLINE,
						get_data_from_worker({unregister_pid, Pid, Login})
				end;
			true ->
				Reply = ?ERROR_USER_NOT_ONLINE,
				get_data_from_worker({unregister_pid, Pid, Login})
		end,
		case Output of
			on_output ->
				hawk_server_lib:send_message(mask(Reply), S, Transport);
			_ ->
				true
		end
	end, Pids),
	{ok, NewCnt} = hawk_server_statistic:add_message(H_name),
	NewCnt.

 %===============================================
'POST_ANSWER'({data, Data}, #state{socket=S, transport=Transport} = State) ->
	case re:run(Data, "Transport\: sokets", [global]) of
		nomatch ->  
			{ok, POST_data} = Transport:recv(S, 0, infinity),
			Post = hawk_server_lib:get_json_from_post(POST_data);
		{match, _} ->
			[_Headers, Msg] = re:split(Data, "\r\n\r\n"),
			Post = hawk_server_lib:split_json_by_part(binary_to_list(Msg))
	end,
    
	case Post of
		false ->
			Res = ?ERROR_UNKNOW_DATA_TYPE;
		{ok, Qtype, StrJSON} ->
			JSON = 
				try 
					get_json_data(hawk_server_lib:convert_to_atom(Qtype), list_to_binary(StrJSON)) of
						J -> J
				catch  _A:_B -> 
					false
				end,

			Res = 
				try 
					action_on_user(JSON, State) of
					R -> R
				catch  _C:_D -> 
					?ERROR_INVALID_FORMAT_DATA
				end
	end,
	
	Frame = hawk_server_lib:convert_to_binary(["\r\n", Res]),
	
	hawk_server_lib:send_message({ok, Frame}, S, Transport),
	Transport:close(S),

	{stop, normal, State}.
 
action_on_user({register_user, Key, Id}, _State) ->
	case check_login_format(Id) of
		true ->
			get_data_from_worker({register_user, Key, Id});
		false ->
			?ERROR_INVALID_LOGIN_FORMAT
	end;

action_on_user({unregister_user, Key, Id}, _State) ->
	case check_login_format(Id) of
		true ->
			get_data_from_worker({unregister_user, Key, Id});
		false ->
			?ERROR_INVALID_LOGIN_FORMAT
	end;

action_on_user({add_domain, Key, Domain, Login}, _State) ->
	atom_to_list(get_data_from_worker({add_domain, Key, Domain, Login}));

action_on_user({del_domain, Key, Domain, Login}, _State) ->
	atom_to_list(get_data_from_worker({del_domain, Key, Domain, Login}));

action_on_user({add_in_groups, Key, Id, Groups}, _State) when is_list(Groups) ->
	get_data_from_worker({add_in_groups, Key, Id, Groups});
action_on_user({add_in_groups, _Key, _Id, _Groups}, _State) ->
	?ERROR_INVALID_GROUP_FORMAT;

action_on_user({remove_from_groups, Key, Id, Groups}, _State) when is_list(Groups) ->
	get_data_from_worker({remove_from_group, Key, Id, Groups});
action_on_user({remove_from_groups, _Key, _Id, _Groups}, _State) ->
	?ERROR_INVALID_GROUP_FORMAT;

action_on_user({send_group_message, Key, Text, Groups, Time, From}, State) when is_list(Groups) ->
	Res = get_data_from_worker({get_by_group_for_message, Key, Groups}),
	Users = get_users_from_groups(Res, From),
	UnUsers = lists:usort(Users),

	lists:foreach(fun(U) ->
			To_data = #message{from=From, to=U, time=Time, text=Text},
			handle_user_message(off_output, get_data_from_worker({get_pids, U}), 0, To_data, U, State)
	end, UnUsers),

	?OK;

action_on_user({send_group_message, _Key, _Text, _Groups, _Time, _From}, _State) ->
	?ERROR_INVALID_GROUP_FORMAT;

action_on_user({get_by_group, Key, Groups}, _State) when is_list(Groups) ->
	Res = hawk_server_lib:flatten(get_data_from_worker({get_by_group, Key, Groups})),
	?list_records_to_json(users_in_group, Res);
action_on_user({get_by_group, _Key, _Groups}, _State) ->
	?ERROR_INVALID_GROUP_FORMAT.	

check_login_format(Data) ->
	case re:run(Data, "^[a-zA-Z0-9]{3,64}$") of
        {match, _} -> 
        	true;
        nomatch ->
        	false
      end.


get_users_from_groups(Groups, From) ->
	Fun = fun(H) ->
		{users_in_group_for_message, _GrpName, Users} = H,
		case lists:member(From, Users) of
			true ->
				Users;
			false ->
				[]
		end
	end,
	lists:flatten(lists:map(Fun, Groups)).

handle_event(Event, StateName, StateData) ->
    {stop, {StateName, undefined_event, Event}, StateData}.
 
handle_sync_event(Event, _From, StateName, StateData) ->
    {stop, {StateName, undefined_event, Event}, StateData}.
 
handle_info({tcp, Socket, Bin}, StateName, #state{socket=Socket} = StateData) ->
    inet:setopts(Socket, [{active, once}]),
   	?MODULE:StateName({data, Bin}, StateData);
 
handle_info({tcp_closed, Socket}, _StateName,
            #state{socket=Socket, login=Login} = StateData) ->
	disconect_user(Login),
    {stop, normal, StateData};
handle_info(Data, StateName, StateData) ->
	 ?MODULE:StateName(Data, StateData).

disconect_user(Login) ->
	get_data_from_worker({unregister_pid, self(), Login}),
    error_logger:info_msg("~p Client ~p disconnected.\n", [self(), Login]).

terminate(_Reason, _StateName, #state{socket=Socket, transport=Transport}) ->
    (catch Transport:close(Socket)),
    ok.
 
code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.

get_awsw_key(Key) ->
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
		(Len >= 126) and (Len =< 65535) ->
			Frame = <<1:1, 0:3, 1:4, 0:1, 126:7, Len:16, Data/binary>>;
		Len > 65536 ->
			Frame = <<1:1, 0:3, 1:4, 0:1, 127:7, Len:64, Data/binary>>;
		true -> 
			Frame = <<1:1, 0:3, 1:4, 0:1, Len:7, Data/binary>>
	end,
	{ok, Frame}.

handle_data(Data) ->
    <<_Fin:1, _Rsv:3, _Opcode:4, _Mask:1, Len:7, Rest/binary>> = Data,
    %Len2 = size(Data),
	if 	
		(Len >= 126) and (Len =< 65535) ->
			<<Dop_len:16/unsigned-integer, Masking:4/binary, Payload:Dop_len/binary, _Next/binary>> = Rest;
		Len > 65536 ->
			<<Dop_len:64/unsigned-integer, Masking:4/binary, Payload:Dop_len/binary, _Next/binary>> = Rest;
		true -> 
			_Dop_len = 0,
			<<Masking:4/binary, Payload:Len/binary, _Next/binary>> = Rest
	end,
    
    Line = unmask(Payload, Masking),
    {ok, Line}.

get_json_data(message, Data) ->
	try 
		?json_to_record(message, Data) of
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
	end;

get_json_data(add_in_groups, Data) ->
	try ?json_to_record(add_in_groups, Data) of
		R -> R
	catch  _:_ -> 
		false
	end;

get_json_data(remove_from_groups, Data) ->
	try ?json_to_record(remove_from_groups, Data) of
		R -> R
	catch  _:_ -> 
		false
	end;

get_json_data(get_by_group, Data) ->
	try ?json_to_record(get_by_group, Data) of
		R -> R
	catch  _:_ -> 
		false
	end;

get_json_data(send_group_message, Data) ->
	try ?json_to_record(send_group_message, Data) of
		R -> R
	catch  _:_ -> 
		false
	end.