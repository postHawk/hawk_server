-module(hawk_server_app).
-author('mbarulin@gmail.com').

-behaviour(application).

-include("env.hrl").
-include("mac.hrl").

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_Type, _StartArgs) ->
	ListenPort = get_app_env(listen_port, ?DEF_PORT),

	application:ensure_started(crypto),
	application:ensure_started(ranch),
	application:ensure_started(bson),
	application:ensure_started(mongodb),
	application:ensure_started(gproc),

	ok = filelib:ensure_dir("data"),

	dets:open_file(reg_users_data, [{access, read_write}, {type, set}, {auto_save, 10000}, {file, "data/reg_users_data"}]),
	dets:open_file(main_user_data, [{access, read_write}, {type, set}, {auto_save, 10000}, {file, "data/main_user_data"}]),
	dets:open_file(groups_to_user, [{access, read_write}, {type, set}, {auto_save, 10000}, {file, "data/groups_to_user"}]),
	dets:open_file(user_to_groups, [{access, read_write}, {type, set}, {auto_save, 10000}, {file, "data/user_to_groups"}]),
	dets:open_file(created_groups, [{access, read_write}, {type, set}, {auto_save, 10000}, {file, "data/created_groups"}]),

	{ok, _} = ranch:start_listener(hawk_pool, 1,
		?TRANSPORT, [
			{port, ListenPort},
			{certfile, "/home/admin/conf/web/ssl.post-hawk.com.pem"},
			{keyfile, "/home/admin/conf/web/ssl.post-hawk.com.key"},
			{cacertfile, "/home/admin/conf/web/sub.class1.server.sha2.ca .pem"},
			{ciphers, ["ECDHE-ECDSA-AES256-SHA384", "ECDHE-RSA-AES256-SHA384",
				"ECDH-ECDSA-AES256-SHA384", "ECDH-RSA-AES256-SHA384",
				"DHE-RSA-AES256-SHA256", "DHE-DSS-AES256-SHA256",
				"AES256-SHA256", "ECDHE-ECDSA-AES128-SHA256",
				"ECDHE-RSA-AES128-SHA256", "ECDH-ECDSA-AES128-SHA256",
				"ECDH-RSA-AES128-SHA256", "DHE-RSA-AES128-SHA256",
				"DHE-DSS-AES128-SHA256", "AES128-SHA256",
				"ECDHE-ECDSA-AES256-SHA", "ECDHE-RSA-AES256-SHA",
				"DHE-RSA-AES256-SHA", "DHE-DSS-AES256-SHA",
				"ECDH-ECDSA-AES256-SHA", "ECDH-RSA-AES256-SHA", "AES256-SHA",
				"ECDHE-ECDSA-DES-CBC3-SHA", "ECDHE-RSA-DES-CBC3-SHA",
				"EDH-RSA-DES-CBC3-SHA", "EDH-DSS-DES-CBC3-SHA"
			]},
			{verify, verify_peer},
			{server_name_indication, {137,135,135,143}}
		], hawk_server_listener, []),
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
				error -> Default
			end
	end.