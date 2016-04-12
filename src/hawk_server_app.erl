%% @author Barulin Maxim <mbarulin@gmail.com>
%% @copyright 2016 Barulin Maxim
%% @version 0.0.3
%% @title hawk_server_app
%% @doc Коневой модуль приложения.
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
%%	ListenPort = get_app_env(listen_port, ?DEF_PORT),

	application:ensure_started(crypto),
	application:ensure_started(bson),
	application:ensure_started(mongodb),
	application:ensure_started(gproc),

	ok = filelib:ensure_dir("data"),

	dets:open_file(main_user_data, [{access, read_write}, {type, set}, {auto_save, 10000}, {file, "data/main_user_data"}]),

	hawk_server_sup:start_link().

stop(_State) ->
	dets:close(main_user_data),
	ok.
