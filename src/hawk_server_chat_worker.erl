-module(hawk_server_chat_worker).
-author('mbarulin@gmail.com').
-behaviour(gen_fsm).

-export([start_link/1]).
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

-record(state,{
	socket,    % client socket
	addr,       % client address
	host_name,
	key,
	curent_login, %process id
	register_login, %register id for user
	transport,
	parent
}).

%%%------------------------------------------------------------------------
%%% API
%%%------------------------------------------------------------------------
 
get_data_from_worker(Params) ->
	gen_server:call(hawk_server_api_manager, Params).

start_link(Parent) ->
    gen_fsm:start_link(?MODULE, [Parent], []).

init([Parent]) -> {ok, 'WAIT_FOR_SOCKET', #state{parent=Parent}}.


'WAIT_FOR_SOCKET'({socket_ready, Socket, H_name, Transport}, State) when is_port(Socket) ->
    % Now we own the socket
    inet:setopts(Socket, [{active, once}, binary]),
	
    {next_state, 'WAIT_FOR_DATA', State#state{
		  socket=Socket, 
		  host_name=H_name, 
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
	
	Key = proplists:get_value(<<"Sec-WebSocket-Key">>, Headers),
	
	case Key of
        undefined -> B_all_answ = ?ERROR_INVALID_HANDSHAKE;
		_ -> {ok, B_all_answ} = get_awsw_key(Key)
    end,

	hawk_server_lib:send_message({ok, B_all_answ}, S, Transport),
	{next_state, 'WAIT_LOGIN_MESSAGE', State}.
 
 %===============================================
'WAIT_LOGIN_MESSAGE'({data, Bin}, State) ->
	{ok, Data} =  handle_data(Bin),
	handle_login_format(check_login_format(Data), Data, State).

handle_login_format(true, User_id, #state{host_name=H_name} = State) ->
	Ch_res = get_data_from_worker({check_user_by_domain, User_id, list_to_binary(lists:flatten(H_name))}),
	handle_login_main_data(Ch_res, User_id, State);

handle_login_format(false, _User_id, #state{socket=S, transport=Transport} = State) ->
	case erlang:port_info(S) of
		undefined ->
			true;
		_ -> 
			hawk_server_lib:send_message(mask(?ERROR_INVALID_LOGIN_FORMAT), S, Transport)
	end,
	{next_state, 'WAIT_LOGIN_MESSAGE', State}.
%==============

handle_login_main_data({ok,false, Reason}, _, #state{socket=S, transport=Transport} = State) ->
	Msg = 
		case Reason of
			no_id -> ?ERROR_USER_NOT_REGISTER;
			no_login -> ?ERROR_INVALID_KEY;
			no_domain -> ?ERROR_DOMAIN_NOT_REGISTER
		end,
	
	hawk_server_lib:send_message(mask(Msg), S, Transport),
  	Transport:close(S),
	{stop, normal, State};

handle_login_main_data({ok,true}, Register_login, #state{socket=S, transport=Transport, host_name=H_name} = State) ->
	RegLogin = {list_to_binary(H_name), Register_login},
	gproc:reg({p,l,RegLogin}, undefined),
	hawk_server_lib:send_message(mask(?OK), S, Transport),
	{next_state, 'WAIT_USER_MESSAGE', State#state{curent_login=RegLogin, register_login=Register_login}}.

%===============================================
'WAIT_USER_MESSAGE'({data, Bin}, #state{socket=S, transport=Transport} = State) ->
	{ok, Data} =  handle_data(Bin),
	
	case jsx:is_json(Data) of
		true ->
			J_data = jsx:decode(Data),
			case J_data of
				false ->
					Transport:close(S),
					{stop, normal, State};
				_ ->
					To = proplists:get_value(<<"to">>, J_data),
					Gr = proplists:get_value(<<"group">>, To),
					U = proplists:get_value(<<"user">>, To),
					handle_json_message({message, {U, Gr}, J_data}, State),
					{next_state, 'WAIT_USER_MESSAGE', State}
			end;
		false ->
			{stop, normal, State}
	end;

'WAIT_USER_MESSAGE'({new_message, Bin}, #state{socket=S, transport=Transport} = State) ->
	?DBG(Bin),
	?DBG(jsx:encode(Bin)),
 	{ok, Frame} = mask(jsx:encode(Bin)),
 	hawk_server_lib:send_message({ok, Frame}, S, Transport),
	
	{next_state, 'WAIT_USER_MESSAGE', State};

'WAIT_USER_MESSAGE'({'EXIT', _Pid, _Reason}, State) ->
	{next_state, 'WAIT_USER_MESSAGE', State}.

handle_json_message({message, {ToUser, undefined}, J_data}, 
					#state{socket=S, host_name=H_name, curent_login=CurentLogin, transport=Transport} = State) ->
	if 
		{H_name, ToUser} =/= CurentLogin ->
 			handle_user_message(on_output, get_data_from_worker({get_pids, ToUser}), J_data, State);
		true ->
			hawk_server_lib:send_message(mask(?ERROR_SEND_MESSAGE_YOURSELF), S, Transport)
	end;

handle_json_message({message, {undefined, ToGrp}, J_data}, 
					#state{socket=S, register_login=Login, transport=Transport} = State) when is_list(ToGrp) ->
    case ets:lookup(reg_users_data, Login) of
        [] -> 
        	Reply = ?ERROR_USER_NOT_REGISTER;
        [{_, MLogin}] ->
            case ets:lookup(main_user_data, MLogin) of
                [] -> 
                	Reply = ?ERROR_GENERAL_ERROR;
                [{_, _, Key}] ->
                    action_on_user({send_group_message, Key, ToGrp, J_data}, State),
                    Reply = ?OK
            end
    end,
    hawk_server_lib:send_message(mask(Reply), S, Transport);

handle_json_message({message, {ToUser, ToGrp}, J_data},  State) when is_list(ToGrp) ->
    handle_json_message({message, {ToUser, undefined}, J_data}, State) ,
    handle_json_message({message, {undefined, ToGrp}, J_data}, State);

handle_json_message({message, _To, J_data}, #state{socket=S, transport=Transport}) ->
	hawk_server_lib:send_message(mask(?ERROR_INVALID_FORMAT_DATA), S, Transport).

 %===============================================

 %===============================================

handle_user_message(Output, [], _To_data, #state{socket=S, transport=Transport} = _State) ->
	case Output of
		on_output ->
			hawk_server_lib:send_message(mask(?ERROR_USER_NOT_ONLINE), S, Transport);
		_ ->
			true
	end;

handle_user_message(Output, Pids, J_data, #state{host_name=H_name, socket=S, transport=Transport} = _State) ->
	%@todo answer in loop is bad for group msg
	lists:foreach(fun(Pid)->
		case is_process_alive(Pid) of
			true ->
				Pid ! {new_message, J_data},
				Reply = ?OK;
			false ->
				Reply = ?ERROR_USER_NOT_ONLINE
		end,
					 
		case Output of
			on_output ->
				hawk_server_lib:send_message(mask(Reply), S, Transport);
			_ ->
				true
		end
	end, Pids),
	{ok, _} = hawk_server_statistic:add_message(H_name).

 %===============================================
'POST_ANSWER'({data, Data}, #state{socket=S, transport=Transport} = State) ->
	{ok, {http_request,_Method,{abs_path, _URL},_}, H} = erlang:decode_packet(http, Data, []),
	[_Headers, {body, POST}] = hawk_server_lib:parse_header(H),
	
	Splitted = hawk_server_lib:split_json_by_part(binary_to_list(POST)),
	
	Res = 
		case Splitted of
			false ->
				?ERROR_UNKNOW_DATA_TYPE;
			{ok, [[Qtype]], [[StrJSON]]} ->
				JSON = jsx:decode(list_to_binary(StrJSON)),
				case Qtype of
					<<"send_group_message">> -> action_on_user({Qtype, JSON}, State) ;
					_ 						 -> action_on_user({Qtype, JSON}) 
				end
		end,
	
	Frame = hawk_server_lib:convert_to_binary(["\r\n", Res]),
	
	hawk_server_lib:send_message({ok, Frame}, S, Transport),
	Transport:close(S),

	{stop, normal, State}.

action_on_user({"register_user", [{<<"key">>, Key}, {<<"id">>, Id}]}) ->
	case check_login_format(Id) of
		true ->
			get_data_from_worker({register_user, Key, Id});
		false ->
			?ERROR_INVALID_LOGIN_FORMAT
	end;

action_on_user({"unregister_user", [{<<"key">>, Key}, {<<"id">>, Id}]}) ->
	case check_login_format(Id) of
		true ->
			get_data_from_worker({unregister_user, Key, Id});
		false ->
			?ERROR_INVALID_LOGIN_FORMAT
	end;

action_on_user({"add_domain", [{<<"key">>, Key}, {<<"domain">>, Domain}, {<<"login">>, Login}]}) ->	
	atom_to_list(get_data_from_worker({add_domain, Key, Domain, Login}));

action_on_user({"del_domain", [{<<"key">>, Key}, {<<"domain">>, Domain}, {<<"login">>, Login}]}) ->
	atom_to_list(get_data_from_worker({del_domain, Key, Domain, Login}));

action_on_user({"add_in_groups", [{<<"key">>, Key}, {<<"id">>, Id}, {<<"groups">>, Groups}]}) when is_list(Groups) ->
	get_data_from_worker({add_in_groups, Key, Id, Groups});

action_on_user({"add_in_groups", _Key, _Id, _Groups}) ->
	?ERROR_INVALID_GROUP_FORMAT;

action_on_user({"remove_from_groups", [{<<"key">>, Key}, {<<"id">>, Id}, {<<"groups">>, Groups}]}) when is_list(Groups) ->
	get_data_from_worker({remove_from_group, Key, Id, Groups});

action_on_user({"remove_from_groups", _Key, _Id, _Groups}) ->
	?ERROR_INVALID_GROUP_FORMAT;

action_on_user({"get_by_group",  [{<<"key">>, Key}, {<<"groups">>, Groups}]}) when is_list(Groups) ->
	Res = get_data_from_worker({get_by_group, Key, Groups}),
	jsx:encode(Res);

action_on_user({"get_by_group", _Msg}) ->
	?ERROR_INVALID_GROUP_FORMAT.

action_on_user({send_group_message, Key, Groups, J_data}, State) when is_list(Groups) ->
	
 	Res = get_data_from_worker({get_by_group_for_message, Key, Groups}),
%% 	Users = get_users_from_groups(Res, From),
%% 	UnUsers = lists:usort(Users),
%% 
%% 	lists:foreach(fun(U) ->
%% 			To_data = #message{from=From, to=U, time=Time, text=Text},
%% 			handle_user_message(off_output, get_data_from_worker({get_pids, U}), 0, To_data, U, State)
%% 	end, UnUsers),

	?OK;

action_on_user({send_group_message, _Key, _Text, _Groups, _Time, _From}, _State) ->
	?ERROR_INVALID_GROUP_FORMAT.

%===============================================
handle_event(Event, StateName, StateData) ->
    {stop, {StateName, undefined_event, Event}, StateData}.
 
handle_sync_event(Event, _From, StateName, StateData) ->
    {stop, {StateName, undefined_event, Event}, StateData}.
 
handle_info({tcp, Socket, Bin}, StateName, #state{socket=Socket} = StateData) ->
    inet:setopts(Socket, [{active, once}]),
   	?MODULE:StateName({data, Bin}, StateData);
 
handle_info({tcp_closed, Socket}, _StateName,
            #state{socket=Socket} = StateData) ->
    {stop, normal, StateData};
handle_info(Data, StateName, StateData) ->
	 ?MODULE:StateName(Data, StateData).

terminate(_Reason, _StateName, #state{socket=Socket, transport=Transport}) ->
    (catch Transport:close(Socket)),
    ok.
 
code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.


%% ====================================================================
%% Internal functions
%% ====================================================================
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

check_login_format(Data) ->
	case re:run(Data, "^[a-zA-Z0-9]{3,64}$") of
        {match, _} -> 
        	true;
        nomatch ->
        	false
      end.


