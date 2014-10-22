-define(MAX_RESTART,    5).
-define(MAX_TIME,      60).
-define(DEF_PORT,    2222).
-define(MAX_MESSAGE_COUNT, 10).
-define(TIMEOUT, 120000).
-define(API_SALT, "dfm@,vadn54/sdfa3;jx").

-ifndef(DBG).
	-define(DBG(Var), io:format("DEBUG: ~p:~p - ~p~n~n ~p~n~n", [?MODULE, ?LINE, ??Var, Var])).
-endif.