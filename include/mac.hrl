-define(MAX_RESTART,    5).
-define(MAX_TIME,      60).
-define(DEF_PORT,    2222).
-define(TIMEOUT, 120000).
-define(API_SALT, "dfm@,vadn54/sdfa3;jx").

-define(ERROR_DOMAIN_NOT_REGISTER, <<"domain_not_register">>).
-define(ERROR_USER_NOT_REGISTER, <<"user_not_register">>).
-define(ERROR_INVALID_KEY, <<"invalid_key">>).
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

-define(OK, <<"ok">>).

%% -define(HOST, <<"127.0.0.1">>).
%% -define(PORT, 27017).
-define(DB_NAME, <<"hawk">>).

-ifndef(DBG).
	-define(DBG(Var), io:format("DEBUG: ~p:~p - ~p~n ~p~n", [?MODULE, ?LINE, ??Var, Var])).
-endif.

-define(get_user_by_domain(Domain), 
	fun(Domain) ->
		{ok, Connection} = mongo:connect (?DB_NAME),
		Res = mongo:find_one(Connection, <<"users">>, {domain, Domain}),
		mc_worker:disconnect(Connection),
		
		case Res of
			{} ->
				{ok, false};
			_ ->
				{ok, Res}
		end
	end(Domain)
).

-define(get_user_by_key(Key), 
	fun(Key) ->
		{ok, Connection} = mongo:connect (?DB_NAME),
		Res = mongo:find_one(Connection, <<"users">>, {key, Key}),
		mc_worker:disconnect(Connection),
		
		case Res of
			{} ->
				{ok, false};
			_ ->
				{U} = Res,
				{ok, U}
		end
	end(Key)
).

-define(get_user_by_login(Login), 
	fun(Login) ->
		{ok, Connection} = mongo:connect (?DB_NAME),
		Res = mongo:find_one(Connection, <<"users">>, {login, Login}),
		mc_worker:disconnect(Connection),
		
		case Res of
			{} ->
				{ok, false};
			_ ->
				{ok, Res}
		end
	end(Login)
).
