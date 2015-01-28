-module(hawk_server_app).
-author('mbarulin@gmail.com').

-behaviour(application).

-include("mac.hrl").

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_Type, _StartArgs) ->
    ListenPort = get_app_env(listen_port, ?DEF_PORT),
	
	application:ensure_started(ranch),
	application:ensure_started(bson),
	application:ensure_started(erlmongo),
	application:ensure_started(gproc),
	
	ets:new(reg_users_data, [ordered_set, public, named_table]),
    ets:new(main_user_data, [ordered_set, public, named_table]),
    ets:new(groups_to_user, [ordered_set, public, named_table]),
	
	{ok, _} = ranch:start_listener(hawk_pool, 1, ranch_tcp, [{port, ListenPort}], hawk_server_listener, []),
	hawk_server_sup:start_link().

stop(_State) ->
    ok.


%% ====================================================================
%% Internal functions
%% ====================================================================

get_app_env(Opt, Default) ->
    case application:get_env(application:get_application(), Opt) of
    {ok, Val} -> Val;
    _ ->
        case init:get_argument(Opt) of
        [[Val | _]] -> Val;
        error       -> Default
        end
    end.