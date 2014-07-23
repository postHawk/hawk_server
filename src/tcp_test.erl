-module(tcp_test).
-export([test/0]).

test() ->
	{ok,S} = gen_tcp:connect({127,0,0,1},3425,[]),
	gen_tcp:send(S,<<"hello">>),
	receive M -> 
		io:format("data ~p", [M]),
		gen_tcp:close(S)
	end.

