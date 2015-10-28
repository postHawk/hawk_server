-module(hawk_server_chat_worker).
-author('mbarulin@gmail.com').
-behaviour(gen_fsm).

-export([start_link/1]).
-import(crypto, [hmac/2]).
-include("env.hrl").
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

-export([handle_req_by_type/4]).

-record(state,{
	socket,    % client socket
	addr,       % client address
	host_name,
	key,
	curent_login,
	register_login, %register id for user
	transport,
	parent
}).

%%%------------------------------------------------------------------------
%%% API
%%%------------------------------------------------------------------------
 
get_data_from_worker(Params) ->
	gen_server:call(hawk_server_api_manager, Params, 30000).

start_link(Parent) ->
    gen_fsm:start_link(?MODULE, [Parent], []).

init([Parent]) -> {ok, 'WAIT_FOR_SOCKET', #state{parent=Parent}}.


'WAIT_FOR_SOCKET'({socket_ready, Socket, H_name, Transport}, State) ->
    % Now we own the socket
    {next_state, 'WAIT_FOR_DATA', State#state{
		  socket=Socket, 
		  host_name=list_to_binary(H_name), 
		  transport=Transport
	 	}};

'WAIT_FOR_SOCKET'(_Other, State) ->
    %error_logger:error_msg("State: 'WAIT_FOR_SOCKET'. Unexpected message: ~p\n", [Other]),
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
    {next_state, 'WAIT_FOR_DATA', State}.

handle_req_by_type(get, Data, S, Transport) ->
	handle_req_by_type(get, Data, #state{socket=S, transport=Transport}).

handle_req_by_type(post, Data, State) ->
	?MODULE:'POST_ANSWER'({data, Data}, State);

 handle_req_by_type(get, Data, #state{socket=S, transport=Transport} = State) ->
	{ok, {http_request,_Method,{abs_path, _URL},_}, H} = erlang:decode_packet(http, Data, []),
	[Headers, {body, _Body}] = hawk_server_lib:parse_header(H),
	
	Key = proplists:get_value(<<"Sec-WebSocket-Key">>, Headers),
	case Key of
        undefined -> B_all_answ = ?get_server_message(<<"handshake">>, ?ERROR_INVALID_HANDSHAKE);
		_ -> {ok, B_all_answ} = get_awsw_key(Key)
    end,

	hawk_server_lib:send_message(false, B_all_answ, S, Transport),
	{next_state, 'WAIT_LOGIN_MESSAGE', State}.
 
 %===============================================
'WAIT_LOGIN_MESSAGE'({data, Bin}, #state{parent=MLogin} = State) ->
	{ok, Data} =  handle_data(Bin),
	{ok, User} = ?get_user_by_login(MLogin),
	handle_login_format(check_login_format(Data), Data, State#state{key=maps:get(<<"key">>, User)}).

handle_login_format(true, User_id, #state{host_name=H_name, key=Key} = State) ->
	Ch_res = get_data_from_worker({check_user_by_domain, Key, User_id, H_name}),
	handle_login_main_data(Ch_res, User_id, State);

handle_login_format(false, _User_id, #state{socket=S, transport=Transport} = State) ->
	hawk_server_lib:send_message(true, ?get_server_message(<<"check_login">>, ?ERROR_INVALID_LOGIN_FORMAT), S, Transport),
	{next_state, 'WAIT_LOGIN_MESSAGE', State}.
%==============

handle_login_main_data({ok,false, Reason}, _, #state{socket=S, transport=Transport} = State) ->
	Msg = 
		case Reason of
			no_id     -> ?get_server_message(<<"check_user">>, ?ERROR_USER_NOT_REGISTER);
			no_login  -> ?get_server_message(<<"check_user">>, ?ERROR_INVALID_API_KEY);
			no_domain -> ?get_server_message(<<"check_user">>, ?ERROR_DOMAIN_NOT_REGISTER)
		end,
	
	hawk_server_lib:send_message(true, Msg, S, Transport),
  	Transport:close(S),
	{stop, normal, State};

handle_login_main_data({ok,true}, Register_login, #state{socket=S, transport=Transport, host_name=H_name} = State) ->
	RegLogin = {H_name, Register_login},
	gproc:reg({p,l,RegLogin}, undefined),
	
	hawk_server_lib:send_message(true, ?get_server_message(<<"check_user">>, false, ?OK), S, Transport),
	
	case hawk_server_queue:get(RegLogin) of
		[{_Key, _Size, List}] ->
			lists:foreach(fun(To_data)->
				handle_user_message(on_output, [self()], Register_login, To_data, State)
			end, List),
			hawk_server_queue:clear(RegLogin);
		[] ->
			true
	end,
	
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
					Action = proplists:get_value(<<"hawk_action">>, J_data),
					handle_json_message({
						Action, get_to_from_record(J_data), J_data
					}, State),
					{next_state, 'WAIT_USER_MESSAGE', State}
			end;
		false ->
			{stop, normal, State}
	end;

'WAIT_USER_MESSAGE'({new_message, Bin}, #state{socket=S, transport=Transport} = State) ->
 	hawk_server_lib:send_message(true, jsx:encode(Bin), S, Transport),
	
	{next_state, 'WAIT_USER_MESSAGE', State};

'WAIT_USER_MESSAGE'({'EXIT', _Pid, _Reason}, State) ->
	{next_state, 'WAIT_USER_MESSAGE', State}.

handle_json_message({<<"send_message">>, {ToUser, undefined}, J_data}, 
					#state{socket=S, host_name=H_name, curent_login=CurentLogin, transport=Transport, key=Key} = State) ->
	Domains = proplists:get_value(<<"domains">>, J_data),
	
	C_j_data = delete_keys([<<"key">>, <<"domains">>], J_data),
	
	if 
		{H_name, ToUser} =/= CurentLogin ->
 			handle_user_message(on_output, get_data_from_worker({get_pids, Key, [ToUser], Domains}), ToUser, C_j_data, State);
		true ->
			hawk_server_lib:send_message(true, ?get_server_message(<<"send_message">>, ?ERROR_SEND_MESSAGE_YOURSELF), S, Transport)
	end;

handle_json_message({<<"send_message">>, {undefined, ToGrp}, J_data}, 
					#state{socket=S, transport=Transport, parent=Mlogin} = State) when is_list(ToGrp) ->
	Login = proplists:get_value(<<"id">>, J_data),
	Reply = case dets:lookup(reg_users_data, {Login, Mlogin}) of
        [] -> 
        	?get_server_message(<<"send_group_message">>, ?ERROR_USER_NOT_REGISTER);
        [{_, MLogin}] ->
            case dets:lookup(main_user_data, MLogin) of
                [] -> 
                	?get_server_message(<<"send_group_message">>, ?ERROR_GENERAL_ERROR);
	            [{_Login, _Domains, Key}] ->
                    api_action({<<"send_group_message">>, [{<<"key">>, Key}|J_data]}, State, on_output),
                    ?get_server_message(<<"send_group_message">>, false, ?OK)
            end
    end,
    hawk_server_lib:send_message(true, Reply, S, Transport);

handle_json_message({<<"send_message">>, {ToUser, ToGrp}, J_data},  State) when is_list(ToGrp) ->
    handle_json_message({<<"send_message">>, {ToUser, undefined}, J_data}, State) ,
    handle_json_message({<<"send_message">>, {undefined, ToGrp}, J_data}, State);

handle_json_message({<<"send_message">>, _To, _J_data}, #state{socket=S, transport=Transport}) ->
	hawk_server_lib:send_message(true, ?get_server_message(<<"send_message">>, ?ERROR_INVALID_FORMAT_DATA), S, Transport);

handle_json_message({<<"get_group_list">>, _To, J_data}, #state{socket=S, transport=Transport, parent=Mlogin}) ->
	Login = proplists:get_value(<<"id">>, J_data),
	Reply = case dets:lookup(reg_users_data, {Login, Mlogin}) of
        [] -> 
        	?get_server_message(<<"get_group_list">>, ?ERROR_USER_NOT_REGISTER);
        [{_, MLogin}] ->
            case dets:lookup(main_user_data, MLogin) of
                [] -> 
                	?get_server_message(<<"get_group_list">>, ?ERROR_GENERAL_ERROR);
                [{_Login, _Domains, Key}] ->
                    Res = get_data_from_worker({
						   get_group_list, 
						   Key, 
						   ?GROUP_ACCESS_PUBLIC, 
						   proplists:get_value(<<"domains">>, J_data)
					  }),

					jsx:encode([{event, proplists:get_value(<<"event">>, J_data)}| ?get_server_message(<<"get_group_list">>, false, Res, false)])
            end
    end,
	
	hawk_server_lib:send_message(true, Reply, S, Transport);

handle_json_message({<<"get_group_by_simple_user">>, _To, J_data}, #state{socket=S, transport=Transport, parent=Mlogin}) ->
	Login = proplists:get_value(<<"id">>, J_data),
	Reply = case dets:lookup(reg_users_data, {Login, Mlogin}) of
        [] -> 
        	?get_server_message(<<"get_group_by_simple_user">>, ?ERROR_USER_NOT_REGISTER);
        [{_, MLogin}] ->
            case dets:lookup(main_user_data, MLogin) of
                [] -> 
                	?get_server_message(<<"get_group_by_simple_user">>, ?ERROR_GENERAL_ERROR);
                [{_Login, _Domains, Key}] ->
                    Res = get_data_from_worker({
						   get_group_by_simple_user, 
						   Key, 
						   Login,
						   ?GROUP_ACCESS_PUBLIC, 
						   proplists:get_value(<<"domains">>, J_data)
					  }),

					jsx:encode([
						{event, proplists:get_value(<<"event">>, J_data)} 
						| ?get_server_message(<<"get_group_by_simple_user">>, false, Res, false)
					])
            end
    end,
	
	hawk_server_lib:send_message(true, Reply, S, Transport);

handle_json_message({<<"add_in_groups">>, _To, J_data}, #state{socket=S, transport=Transport, parent=Mlogin}) ->
	Login = proplists:get_value(<<"id">>, J_data),
	Reply = case dets:lookup(reg_users_data, {Login, Mlogin}) of
        [] -> 
        	?get_server_message(<<"add_in_groups">>, ?ERROR_USER_NOT_REGISTER);
        [{_, MLogin}] ->
            case dets:lookup(main_user_data, MLogin) of
                [] -> 
                	?get_server_message(<<"add_in_groups">>, ?ERROR_GENERAL_ERROR);
                [{_Login, _Domains, Key}] ->
                    Res = get_data_from_worker({
					   add_in_groups, 
					   Key, 
					   Login, 
					   proplists:get_value(<<"groups">>, J_data),
					   proplists:get_value(<<"domains">>, J_data),
					   ?GROUP_ACCESS_PUBLIC
					  }),
					jsx:encode([{event, proplists:get_value(<<"event">>, J_data)}|jsx:decode(Res)])
            end
    end,
	
	hawk_server_lib:send_message(true, Reply, S, Transport);

handle_json_message({<<"remove_from_groups">>, _To, J_data}, #state{socket=S, transport=Transport, parent=Mlogin}) ->
	Login = proplists:get_value(<<"id">>, J_data),
	Reply = case dets:lookup(reg_users_data, {Login, Mlogin}) of
        [] -> 
        	?get_server_message(<<"remove_from_groups">>, ?ERROR_USER_NOT_REGISTER);
        [{_, MLogin}] ->
            case dets:lookup(main_user_data, MLogin) of
                [] -> 
                	?get_server_message(<<"remove_from_groups">>, ?ERROR_GENERAL_ERROR);
                [{_Login, _Domains, Key}] ->
                    Res = get_data_from_worker({
					   remove_from_group, 
					   Key, 
					   Login, 
					   proplists:get_value(<<"groups">>, J_data),
					   proplists:get_value(<<"domains">>, J_data),
					   ?GROUP_ACCESS_PUBLIC
					  }),
					jsx:encode([{event, proplists:get_value(<<"event">>, J_data)}|jsx:decode(Res)])
            end
    end,
	
	hawk_server_lib:send_message(true, Reply, S, Transport);

handle_json_message({<<"get_by_group">>, _To, J_data}, #state{socket=S, transport=Transport, parent=Mlogin}) ->
	Login = proplists:get_value(<<"id">>, J_data),
	Reply =
		case dets:lookup(reg_users_data, {Login, Mlogin}) of
			[] ->
				?get_server_message(<<"get_by_group">>, ?ERROR_USER_NOT_REGISTER);
			[{_, MLogin}] ->
				case dets:lookup(main_user_data, MLogin) of
					[] ->
						?get_server_message(<<"get_by_group">>, ?ERROR_GENERAL_ERROR);
					[{_Login, _Domains, Key}] ->
						Res = get_data_from_worker({
							get_by_group,
							Key,
							proplists:get_value(<<"groups">>, J_data),
							proplists:get_value(<<"domains">>, J_data),
							?GROUP_ACCESS_PUBLIC
						}),

						case Res of
							false ->
								?get_server_message(<<"get_by_group">>, ?ERROR_INVALID_GROUP_COUNT);
							_ ->
								jsx:encode([{event, proplists:get_value(<<"event">>, J_data)}|?get_server_message(<<"get_by_group">>, false, Res, false)])
						end
				end
		end,

	hawk_server_lib:send_message(true, Reply, S, Transport).

 %===============================================

 %===============================================

handle_user_message(Output, [], User, J_data, #state{socket=S, transport=Transport, host_name=Host} = _State) ->
	case Output of
		on_output ->
			Key = {Host, User},
			case hawk_server_queue:add(Key, J_data) of
				false ->
					hawk_server_queue:new(Key, 5),
					hawk_server_queue:add(Key, J_data);
				ok ->
					true
			end,
			hawk_server_lib:send_message(true, ?get_server_message(<<"send_message">>, ?ERROR_USER_NOT_ONLINE), S, Transport);
		_ ->
			true
	end;

handle_user_message(Output, Pids, _User, J_data, #state{host_name=H_name, socket=S, transport=Transport} = _State) ->
	lists:foreach(fun(Pid)->
		Reply = hawk_server_lib:send_message_to_pid(Pid, J_data),
		case Output of
			on_output ->
				hawk_server_lib:send_message(true, jsx:encode([{event, proplists:get_value(<<"event">>, J_data)}|Reply]), S, Transport);
			_ ->
				true
		end
	end, Pids),
	{ok, _} = hawk_server_statistic:add_message(H_name).

 %===============================================
'POST_ANSWER'({data, Data}, #state{socket=S, transport=Transport} = State) ->
	{ok, {http_request,_Method,{abs_path, _URL},_}, H} = erlang:decode_packet(http, Data, []),
	[_Headers, {body, POST}] = hawk_server_lib:parse_header(H),

	Splitted = hawk_server_lib:split_json_by_part(POST),
	
	Res = 
		case Splitted of
			false ->
				?get_server_message(<<"check_data">>, ?ERROR_UNKNOW_DATA_TYPE);
			{ok, Qtype, JSON} ->
				case Qtype of
					<<"send_group_message">> -> api_action({Qtype, JSON}, State) ;
					<<"send_message">> -> api_action({Qtype, JSON}, State) ;
					_ -> api_action({Qtype, JSON}) 
				end
		end,
	
	Frame = hawk_server_lib:convert_to_binary(["\r\n", Res]),

	hawk_server_lib:send_message(false, Frame, S, Transport),
	Transport:close(S),

	{stop, normal, State}.
%===============================================
%===========ACTION ON USER
%===============================================
api_action({<<"register_user">>, J_data}) ->
	Key = proplists:get_value(<<"key">>, J_data),
	Id = proplists:get_value(<<"id">>, J_data),
	
	case check_login_format(Id) of
		true ->
			get_data_from_worker({register_user, Key, Id});
		false ->
			?get_server_message(<<"register_user">>, ?ERROR_INVALID_LOGIN_FORMAT)
	end;

api_action({<<"unregister_user">>, J_data}) ->
	Key = proplists:get_value(<<"key">>, J_data),
	Id = proplists:get_value(<<"id">>, J_data),
	case check_login_format(Id) of
		true ->
			get_data_from_worker({unregister_user, Key, Id});
		false ->
			?get_server_message(<<"unregister_user">>, ?ERROR_INVALID_LOGIN_FORMAT)
	end;
%===============================================
%===========ACTION ON DOMAINS
%===============================================
api_action({<<"add_domain">>, J_data}) ->
	Key = proplists:get_value(<<"key">>, J_data),
	Domain = proplists:get_value(<<"domain">>, J_data),
	Login = proplists:get_value(<<"login">>, J_data),
	get_data_from_worker({add_domain, Key, Domain, Login});

api_action({<<"del_domain">>, J_data}) ->
	Key = proplists:get_value(<<"key">>, J_data),
	Domain = proplists:get_value(<<"domain">>, J_data),
	Login = proplists:get_value(<<"Login">>, J_data),
	get_data_from_worker({del_domain, Key, Domain, Login});
%===============================================
%===========ACTION ON CHANELS
%===============================================
api_action({<<"add_chanel">>, J_data}) ->
	Key = proplists:get_value(<<"key">>, J_data),
	Name = proplists:get_value(<<"name">>, J_data),
	Access = proplists:get_value(<<"access">>, J_data),
	Domains = proplists:get_value(<<"domains">>, J_data),
	get_data_from_worker({add_chanel, Key, Name,  Access, Domains});

api_action({<<"remove_chanel">>, J_data}) ->
	Key = proplists:get_value(<<"key">>, J_data),
	Name = proplists:get_value(<<"name">>, J_data),
	Access = proplists:get_value(<<"access">>, J_data),
	Domains = proplists:get_value(<<"domains">>, J_data),
	get_data_from_worker({remove_chanel, Key, Name, Access, Domains});

%===============================================
%===========ACTION ON GROUPS
%===============================================
api_action({<<"add_in_groups">>, J_data}) ->
	Key = proplists:get_value(<<"key">>, J_data),
	Id = proplists:get_value(<<"id">>, J_data),
	Groups = proplists:get_value(<<"groups">>, J_data),
	Domains = proplists:get_value(<<"domains">>, J_data),

	if
		is_list(Groups) -> get_data_from_worker({add_in_groups, Key, Id, Groups, Domains, ?GROUP_ACCESS_ALL});
		true -> ?get_server_message(<<"add_in_groups">>, ?ERROR_INVALID_GROUP_FORMAT)
	end;

api_action({<<"remove_from_groups">>, J_data}) ->
	Key = proplists:get_value(<<"key">>, J_data),
	Id = proplists:get_value(<<"id">>, J_data),
	Groups = proplists:get_value(<<"groups">>, J_data),
	Domains = proplists:get_value(<<"domains">>, J_data),
	
	if
		is_list(Groups) -> get_data_from_worker({remove_from_group, Key, Id, Groups, Domains, ?GROUP_ACCESS_ALL});
		true -> ?get_server_message(<<"remove_from_groups">>, ?ERROR_INVALID_GROUP_FORMAT)
	end;

api_action({<<"add_groups">>, J_data}) ->
	Key = proplists:get_value(<<"key">>, J_data),
	Groups = proplists:get_value(<<"groups">>, J_data),
	Domains = proplists:get_value(<<"domains">>, J_data),
	get_data_from_worker({add_groups, Key, Groups, Domains});

api_action({<<"remove_groups">>, J_data}) ->
	Key = proplists:get_value(<<"key">>, J_data),
	Groups = proplists:get_value(<<"groups">>, J_data),
	Domains = proplists:get_value(<<"domains">>, J_data),
	get_data_from_worker({remove_groups, Key, Groups, Domains});

api_action({<<"get_group_list">>, J_data}) ->
	Key = proplists:get_value(<<"key">>, J_data),
	Access = proplists:get_value(<<"access">>, J_data),
	Domains = proplists:get_value(<<"domains">>, J_data),
	Groups = get_data_from_worker({get_group_list, Key, Access, Domains}),
	jsx:encode(Groups);

%список групп по простому пользователю
api_action({<<"get_group_by_simple_user">>, J_data}) ->
	Key = proplists:get_value(<<"key">>, J_data),
	Access = proplists:get_value(<<"access">>, J_data),
	Domains = proplists:get_value(<<"domains">>, J_data),
	Login = proplists:get_value(<<"login">>, J_data),

	Groups = get_data_from_worker({get_group_by_simple_user, Key, Login, Access, Domains}),
	jsx:encode(Groups);	

api_action({<<"get_by_group">>,  J_data}) ->
	Key = proplists:get_value(<<"key">>, J_data),
	Groups = proplists:get_value(<<"groups">>, J_data),
	Domains = proplists:get_value(<<"domains">>, J_data),

	if
		is_list(Groups) -> 
			case get_data_from_worker({get_by_group, Key, Groups, Domains, ?GROUP_ACCESS_ALL}) of
				false ->
					?get_server_message(<<"get_by_group">>, ?ERROR_INVALID_GROUP_COUNT);
				Res ->
					jsx:encode(Res)
			end;
		true -> 
			?get_server_message(<<"get_by_group">>, ?ERROR_INVALID_GROUP_FORMAT)
	end.

api_action({<<"send_group_message">>, J_data}, State) ->
	api_action({"send_group_message", J_data}, State, off_output);

api_action({<<"send_message">>, J_data}, #state{key=Key} = State) ->
	To = proplists:get_value(<<"to">>, J_data),
	Domains = proplists:get_value(<<"domains">>, J_data),
	C_j_data = delete_keys([<<"key">>, <<"domains">>], J_data),
	
	handle_user_message(off_output, get_data_from_worker({get_pids, Key, [To], Domains}), To, C_j_data, State),
	?get_server_message(<<"send_message">>, false, ?OK).

api_action({<<"send_group_message">>, J_data}, #state{parent=Parent} = State, Output) ->
	Key = proplists:get_value(<<"key">>, J_data),
	From = proplists:get_value(<<"from">>, J_data),
	Text = proplists:get_value(<<"text">>, J_data),
	Domains = proplists:get_value(<<"domains">>, J_data),
	Event = proplists:get_value(<<"event">>, J_data),


	Groups =
		case proplists:get_value(<<"groups">>, J_data) of
			undefined ->
				To = proplists:get_value(<<"to">>, J_data),
				proplists:get_value(<<"group">>, To);
			Gr ->
				Gr
		end,
	
	Check = if	
		Parent =/= <<"post_sup">> ->
			Restriction = ?GROUP_ACCESS_PUBLIC,
			get_data_from_worker({check_user_domains, Key, Domains, Parent});
		true ->
			Restriction = ?GROUP_ACCESS_ALL,
			true
	end,
	
	if
		is_list(Groups) andalso is_list(Domains) andalso Check == true ->
			Res = get_data_from_worker({get_by_group, Key, Groups, Domains, Restriction}),
			%@todo код крайне не оптимален, для больших групп могут наблюдаться проблемы производительности
			hawk_server_lib:loop_lists(fun(Dom, G, Record) ->
				Acc = get_group_access(G, Dom),

				Allow = if
					Acc == false -> false;
					Acc == ?GROUP_ACCESS_PUBLIC -> true;
					Acc == ?GROUP_ACCESS_PRIVATE -> get_data_from_worker({is_user_in_group, Key, From, G, Dom});
					true -> false
				end,

				case Allow of
					true ->
						Group = proplists:get_value(G, Record),
						lists:foreach(fun(U) ->
							Ulogin = proplists:get_value(user, U),
							O = proplists:get_value(online, U),
							if
								O ->
									To_data = [{from, From}, {to_user, Ulogin}, {to_group, G}, {text, Text}, {event, Event}],
									handle_user_message(
										Output,
										get_data_from_worker({get_pids, Key, [Ulogin], [Dom]}),
										Ulogin,
										To_data,
										State
									);
								true -> true
							end
						end, proplists:get_value(users, Group));
					false ->
						?get_server_message(<<"send_group_message">>, ?ERROR_ACCESS_DENIED_TO_GROUP)
				end
			end, [Domains, Groups, Res]),

			?get_server_message(<<"send_group_message">>, false, ?OK);
		true -> 
			?get_server_message(<<"send_group_message">>, ?ERROR_INVALID_GROUP_FORMAT)
	end.

%===============================================
handle_event(Event, StateName, StateData) ->
    {stop, {StateName, undefined_event, Event}, StateData}.
 
handle_sync_event(Event, _From, StateName, StateData) ->
    {stop, {StateName, undefined_event, Event}, StateData}.
 
handle_info({?PROTOCOL, Socket, Bin}, StateName, #state{socket=Socket, transport=Transport} = StateData) ->
    Transport:setopts(Socket, [{active, once}]),
   	?MODULE:StateName({data, Bin}, StateData);
 
handle_info({?PROTOCOL_CLOSE, Socket}, _StateName,
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

get_group_access(G, Dom) ->
	case dets:lookup(groups_to_user, {G, Dom}) of
		[] -> false;
		[{_Key, _Users, Access}] -> Access  
	end.

delete_keys([], List) ->
	List;

delete_keys([Key|T] = _Keys, List) ->
	New_l = proplists:delete(Key, List),
	delete_keys(T, New_l).

get_to_from_record(J_data) ->
	case proplists:get_value(<<"to">>, J_data) of
		undefined ->
			Gr = undefined,
			U = undefined;
		To ->
			Gr = proplists:get_value(<<"group">>, To),
			U = proplists:get_value(<<"user">>, To)
	end,
	{U, Gr}.



