-module(tcp_server_app).
-author('mbarulin@gmail.com').
 
-behaviour(application).
 
%% Internal API
-export([start_domain_supervisor/1]).
 
%% Application and Supervisor callbacks
-export([start/2, stop/1, init/1]).
 
-define(MAX_RESTART,    5).
-define(MAX_TIME,      60).
-define(DEF_PORT,    2222).
 
%% A startup function for spawning new client connection handling FSM.
%% To be called by the TCP listener process.
start_client(S_name) ->
    supervisor:start_child({global, S_name}, []).

start_domain_supervisor(Domain) ->
  %io:format("exist ~p\n", [global:whereis_name(Domain)]),
  case global:whereis_name(Domain) of
    undefined ->
      {ok, Pid} = supervisor:start_child({global, tcp_client_sup}, []),
      global:unregister_name(domain_sup),
      global:register_name(Domain, Pid);
    _ ->
      true
  end,
	
start_client(Domain).
 
%%----------------------------------------------------------------------
%% Application behaviour callbacks
%%----------------------------------------------------------------------
start(_Type, [Args|_]) ->
%io:format("2\n"),
    ListenPort = get_app_env(listen_port, ?DEF_PORT),
    supervisor:start_link({global, ?MODULE}, ?MODULE, [ListenPort, Args]).
 
stop(_S) ->
    ok.
 
%%----------------------------------------------------------------------
%% Supervisor behaviour callbacks
%%----------------------------------------------------------------------
init([Port, Module]) ->
%io:format("3\n"),
    {ok,
        {_SupFlags = {one_for_one, ?MAX_RESTART, ?MAX_TIME},
            [
              % TCP Listener
              {   tcp_server_sup,                          % Id       = internal id
                  {tcp_listener,start_link,[Port,Module]}, % StartFun = {M, F, A}
                  permanent,                               % Restart  = permanent | transient | temporary
                  2000,                                    % Shutdown = brutal_kill | int() >= 0 | infinity
                  worker,                                  % Type     = worker | supervisor
                  [tcp_listener]                           % Modules  = [Module] | dynamic
              },
              % Client instance supervisor
              {   tcp_client_sup,
                  {supervisor,start_link,[{global, tcp_client_sup}, ?MODULE, [Module]]},
                  permanent,                               % Restart  = permanent | transient | temporary
                  infinity,                                % Shutdown = brutal_kill | int() >= 0 | infinity
                  supervisor,                              % Type     = worker | supervisor
                  []                                       % Modules  = [Module] | dynamic
              },
			  % satistics
              {   statistic_server,
                  {statistic_server,start_link,[]},
                  permanent,                               % Restart  = permanent | transient | temporary
                  2000,                                % Shutdown = brutal_kill | int() >= 0 | infinity
                  worker,                              % Type     = worker | supervisor
                  [statistic_server]                                       % Modules  = [Module] | dynamic
              }
            ]
        }
    };
 
init([Module]) ->
%io:format("1\n"),
    {ok,
        {_SupFlags = {simple_one_for_one, ?MAX_RESTART, ?MAX_TIME},
            [
              % TCP Client
              {   domain_sup,                               % Id       = internal id
                  {supervisor,start_link,[{global, domain_sup}, domain_sup, [Module]]},                  % StartFun = {M, F, A}
                  permanent,                               % Restart  = permanent | transient | temporary
                  infinity,                                    % Shutdown = brutal_kill | int() >= 0 | infinity
                  supervisor,                                  % Type     = worker | supervisor
                  []                                       % Modules  = [Module] | dynamic
              }
            ]
        }
    }.
 
%%----------------------------------------------------------------------
%% Internal functions
%%----------------------------------------------------------------------

get_app_env(Opt, Default) ->
    case application:get_env(application:get_application(), Opt) of
    {ok, Val} -> Val;
    _ ->
        case init:get_argument(Opt) of
        [[Val | _]] -> Val;
        error       -> Default
        end
    end.
	


