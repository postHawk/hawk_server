-define(ENV, dev).

-define(TRANSPORT, ranch_tcp).
-define(PROTOCOL, ssl).
-define(PROTOCOL_CLOSE, tcp_closed).
-define(PROTOCOL_ERROR, tcp_error).

-ifndef(DEF_PORT).
	-define(DEF_PORT,    2223).
-endif.