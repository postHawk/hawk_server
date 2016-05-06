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
-define(ERROR_INVALID_TOKEN, <<"invalid_token">>).

-define(OK, <<"ok">>).

-define(GROUP_ACCESS_PUBLIC, <<"public">>).
-define(GROUP_ACCESS_PRIVATE, <<"private">>).
-define(GROUP_ACCESS_ALL, <<"all">>).
-define(DEFAULT_GROUP_ACCESS, ?GROUP_ACCESS_PUBLIC).

-define(DB_NAME, [{database, <<"hawk">>}]).

-ifndef(DBG).
	-define(DBG(Var), io:format("DEBUG: ~p:~p - ~p~n ~p~n", [?MODULE, ?LINE, ??Var, Var])).
-endif.

