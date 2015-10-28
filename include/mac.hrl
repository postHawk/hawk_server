-define(MAX_RESTART,    5).
-define(MAX_TIME,      60).
-define(TIMEOUT, 120000).
-define(API_SALT, "dfm@,vadn54/sdfa3;jx").

-define(SERVER_REPLY, [{from, hawk_server}, {action, <<"">>}, {result, <<"">>}, {error, false}]).

-define(ERROR_DOMAIN_NOT_REGISTER, <<"domain_not_register">>).
-define(ERROR_USER_NOT_REGISTER, <<"user_not_register">>).
-define(ERROR_INVALID_API_KEY, <<"invalid_api_key">>).
-define(ERROR_INVALID_LOGIN_FORMAT, <<"invalid_login_format">>).
-define(ERROR_INVALID_HANDSHAKE, <<"invalid_handshake">>).
-define(ERROR_INVALID_LOGIN_DATA, <<"invalid_login_data">>).
-define(ERROR_SEND_MESSAGE_YOURSELF, <<"send_message_yourself">>).
-define(ERROR_GENERAL_ERROR, <<"general_error">>).
-define(ERROR_INVALID_FORMAT_DATA, <<"invalid_format_data">>).
-define(ERROR_USER_NOT_ONLINE, <<"user_not_online">>).
-define(ERROR_UNKNOW_DATA_TYPE, <<"unknow_data_type">>).
-define(ERROR_INVALID_GROUP_FORMAT, <<"invalid_group_format">>).
-define(ERROR_INVALID_GROUP_COUNT, <<"invalid_group_count">>).
-define(ERROR_ACCESS_DENIED_TO_GROUP, <<"access_denied_to_group">>).

-define(OK, <<"ok">>).

-define(GROUP_ACCESS_PUBLIC, <<"public">>).
-define(GROUP_ACCESS_PRIVATE, <<"private">>).
-define(GROUP_ACCESS_ALL, <<"all">>).
-define(DEFAULT_GROUP_ACCESS, ?GROUP_ACCESS_PUBLIC).

%% -define(HOST, <<"127.0.0.1">>).
%% -define(PORT, 27017).
-define(DB_NAME, [{database, <<"hawk">>}]).

-ifndef(DBG).
	-define(DBG(Var), io:format("DEBUG: ~p:~p - ~p~n ~p~n", [?MODULE, ?LINE, ??Var, Var])).
-endif.

-define(get_user_by_domain(Domain), 
	fun(D) ->
		?get_user_from_mongo({domain, D})
	end(Domain)
).

-define(get_user_by_key(Key), 
	fun(K) ->
		?get_user_from_mongo({key, K})
	end(Key)
).

-define(get_user_by_login(Login), 
	fun(L) ->
		?get_user_from_mongo({login, L})
	end(Login)
).

-define(get_user_from_mongo(Criteria), 
	fun(Cr) ->
		{ok, Connection} = mongo:connect (?DB_NAME),
		Res = mongo:find_one(Connection, <<"users">>, Cr),
		mc_worker:disconnect(Connection),
		
		case maps:size(Res) of
			0 ->
				{ok, false};
			_ ->
				{ok, Res}
		end
	end(Criteria)
).

-define(get_server_message(Action, Error), 
	fun(A, E) ->
		?get_server_message(A, E, <<"">>, true)
	end(Action, Error)
).

-define(get_server_message(Action, Error, Result), 
	fun(A, E, R) ->
		?get_server_message(A, E, R, true)
	end(Action, Error, Result)
).

-define(get_server_message(Action, Error, Result, Encode), 
	fun(A, E, R, Enc) ->
		R1 = lists:keyreplace(action, 1, ?SERVER_REPLY, {action, A}),
		R2 = lists:keyreplace(error, 1, R1, {error, E}),
		R3 = lists:keyreplace(result, 1, R2, {result, R}),
		
		if 
			Enc == true -> jsx:encode(R3);
			true -> R3
		end
	
	end(Action, Error, Result, Encode)
).

-define(proplist_to_record(Record, List),
	fun(R, L) ->
		list_to_tuple([Record | [proplists:get_value(X, L)
			|| X <- record_info(fields, R)]])
	end(Record, List)
).