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
	application:ensure_started(mongodb),
	application:ensure_started(gproc),
	
	ok = filelib:ensure_dir("data"),
	
	dets:open_file(reg_users_data, [{access, read_write}, {type, set}, {auto_save, 10000}, {file, "data/reg_users_data"}, {ram_file, true}]),
	dets:open_file(main_user_data, [{access, read_write}, {type, set}, {auto_save, 10000}, {file, "data/main_user_data"}, {ram_file, true}]),
	dets:open_file(groups_to_user, [{access, read_write}, {type, set}, {auto_save, 10000}, {file, "data/groups_to_user"}, {ram_file, true}]),
	dets:open_file(user_to_groups, [{access, read_write}, {type, set}, {auto_save, 10000}, {file, "data/user_to_groups"}, {ram_file, true}]),
	dets:open_file(created_groups, [{access, read_write}, {type, set}, {auto_save, 10000}, {file, "data/created_groups"}, {ram_file, true}]),
	
	{ok, _} = ranch:start_listener(hawk_pool, 1, ranch_tcp, [{port, ListenPort}], hawk_server_listener, []),
	hawk_server_sup:start_link().

stop(_State) ->
	dets:close(reg_users_data),
	dets:close(main_user_data),
	dets:close(groups_to_user),
	dets:close(user_to_groups),
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