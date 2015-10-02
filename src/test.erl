%%%-------------------------------------------------------------------
%%% @author maximilian
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. Авг. 2015 16:06
%%%-------------------------------------------------------------------
-module(test).
-author("maximilian").

-include("mac.hrl").

%% API
-export([mongo_test/0]).

  mongo_test() ->
  {ok, Connection} = mongo:connect ([
    {database, <<"hawk">>},
    {login, <<"hawk">>},
    {password, <<"sd879fkalskd2,af^.m52f">>}
  ]),
  Res = mongo:find_one(Connection, <<"users">>, {login, <<"Slavenin">>}),
  mc_worker:disconnect(Connection),
  ?DBG(Res)
.
