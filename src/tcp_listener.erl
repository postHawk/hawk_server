-module(tcp_listener).
-author('saleyn@gmail.com').
 
-behaviour(gen_server).
 
%% External API
-export([start_link/2]).
 
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
        code_change/3, 
        rergister_user/2, unregister_user/2, is_user_exists/1, 
        add_user_pid/2, delete_user_pid/2, get_user_pids/1,
        add_domain/3, del_domain/3, is_user_domain_exists/2
    ]).
 
-record(state, {
                listener,       % Listening socket
                acceptor,       % Asynchronous acceptor's internal reference
                module,          % FSM handling module
                reg_users_data_tableId,
                users_pid_tableId,
                main_user_data
               }).

-record(users, {
    login,
    password,
    email,
    activation_code,
    key,
    domain
}).

-define(API_SALT, "dfm@,vadn54/sdfa3;jx").


 
%%--------------------------------------------------------------------
%% @spec (Port::integer(), Module) -> {ok, Pid} | {error, Reason}
%
%% @doc Called by a supervisor to start the listening process.
%% @end
%%----------------------------------------------------------------------
start_link(Port, Module) when is_integer(Port), is_atom(Module) ->
%io:format("4\n"),
    gen_server:start_link({global, ?MODULE}, ?MODULE, [Port, Module], []).
 
%%%------------------------------------------------------------------------
%%% Callback functions from gen_server
%%%------------------------------------------------------------------------
 
%%----------------------------------------------------------------------
%% @spec (Port::integer()) -> {ok, State}           |
%%                            {ok, State, Timeout}  |
%%                            ignore                |
%%                            {stop, Reason}
%%
%% @doc Called by gen_server framework at process startup.
%%      Create listening socket.
%% @end
%%----------------------------------------------------------------------
init([Port, Module]) ->
%io:format("5\n"),
    process_flag(trap_exit, true),
    Opts = [binary, {reuseaddr, true},
            {keepalive, true}, {backlog, 30}, {active, false}],
    case gen_tcp:listen(Port, Opts) of
        {ok, Listen_socket} ->
            %%Create first accepting process
            {ok, Ref} = prim_inet:async_accept(Listen_socket, -1),
			
            HTableId = ets:new(reg_users_data, [ordered_set]),
            UsPidTableId = ets:new(users_pids, [ordered_set]),
            MUTableId = ets:new(main_user_data, [ordered_set]),
			
            mongoapi:recinfo(#users{}, record_info(fields, users)),
            {ok, #state{listener = Listen_socket,
                        acceptor = Ref,
                        module   = Module,
                        reg_users_data_tableId  = HTableId,
                        users_pid_tableId = UsPidTableId,
                        main_user_data = MUTableId
                    }};
        {error, Reason} ->
            {stop, Reason}
    end.
 
%%-------------------------------------------------------------------------
%% @spec (Request, From, State) -> {reply, Reply, State}          |
%%                                 {reply, Reply, State, Timeout} |
%%                                 {noreply, State}               |
%%                                 {noreply, State, Timeout}      |
%%                                 {stop, Reason, Reply, State}   |
%%                                 {stop, Reason, State}
%% @doc Callback for synchronous server calls.  If `{stop, ...}' tuple
%%      is returned, the server is stopped and `terminate/2' is called.
%% @end
%% @private
%%-------------------------------------------------------------------------
handle_call({rergister_user, Key, Id}, _From, #state{reg_users_data_tableId=TableId, main_user_data=MUTid} = State) ->
    {ok, User} = get_user_by_key(Key),
    %io:format("~p user: ~p\n", [self(), User]),
    case User of
        [] ->
            Reply = false;
        _ ->
			#users{domain=Host, login=Login} = User,
            %регистрируем данные учётки пользователя сайта
            case ets:lookup(MUTid, Login) of
                [] ->
                    {array, List} = Host,
                    ets:insert(MUTid, {Login, List});
                    %io:format("~p add main data: ~p\n", [self(), {Login, List}]);
                _ ->
                    true
            end,

            %регистрируем пользователя для обеспечения возможности пересылки сообщений
            %и связываем его с конкретной учёткой на сайте через логин
            Key_t = binary_to_atom(Id, utf8) ,
            case ets:lookup(TableId, Key_t) of
                [] ->
                    ets:insert(TableId, {Key_t, Login});
                    %io:format("~p register user: ~p\n", [self(), {Key_t, Login}]);
                _ ->
                    true
            end,
            
            Reply = ok
    end,
    {reply, Reply, State};

handle_call({unregister_user, Key, Id}, _From, #state{reg_users_data_tableId=TableId} = State) ->
    {ok, User} = get_user_by_key(Key),
   
    case User of
        [] ->
            Reply = false;
        _ ->
            Key_t = binary_to_atom(Id, utf8) ,

            case ets:lookup(TableId, Key_t) of
                [] ->
                   true;
                _ ->
                    ets:delete(TableId, Key_t)
                    %io:format("~p remove user: ~p\n", [self(), Key_t])
            end,
            
            Reply = ok
    end,
    {reply, Reply, State};

handle_call({check_user, Id}, _From, #state{reg_users_data_tableId=TableId} = State) ->
	case ets:lookup(TableId, Id) of
        [] ->
            {reply, {ok, false}, State};
        _ ->
            {reply, {ok, true}, State}
    end;

handle_call({check_user_domain, Id, Domain}, _From, #state{reg_users_data_tableId=TableId, main_user_data=MUTid} = State) ->

    %io:format("~p Domain ~p ~p\n", [self(), Domain, ets:lookup(TableId, Id)]),

    case ets:lookup(TableId, Id) of
        [] ->
            {reply, {ok, false}, State};
        [{_, Login}] ->
            %io:format("~p user ~p ~p \n", [self(), Id, ets:lookup(MUTid, Login) ]),
            case ets:lookup(MUTid, Login) of
                [] -> 
                    {reply, {ok, false}, State};
                [{_, Hosts}] ->
                    %io:format("~p host ~p \n", [self(), lists:member(Domain, Hosts) ]),
                    case lists:member(Domain, Hosts) of
                        true ->
                            {reply, {ok, true}, State};
                        false ->
                            {reply, {ok, false}, State}
                    end
            end
    end;    

handle_call({add_domain, Key, Domain, Login}, _From, #state{main_user_data=MUTid} = State) ->
    
    Kur_key = get_client_api_key(),
    if 
        Key == Kur_key ->
            case ets:lookup(MUTid, Login) of
                [] -> 
                    true;
                [{_, Hosts}] ->
                    NewList = lists:append([Hosts, [Domain]]),
                    %io:format("~p add domain: ~p:~p all ~p\n", [self(), Login, Domain, NewList]),
                    ets:insert(MUTid, {Login, NewList})
            end,
            Res = ok;
        true ->
            %io:format("~p invalid api key erl: ~p php ~p\n", [self(), Kur_key, Key]),
            Res = false
    end,

    {reply, Res, State};  

handle_call({del_domain, Key, Domain, Login}, _From, #state{main_user_data=MUTid} = State) ->
    Kur_key = get_client_api_key(),
    if 
        Key == Kur_key ->
            case ets:lookup(MUTid, Login) of
                [] -> 
                    true;
                [{_, Hosts}] ->
                    NewList = lists:delete(Domain, Hosts),
                    %io:format("~p remove domain: ~p Remaining ~p\n", [self(), Domain, NewList]),
                    ets:insert(MUTid, {Login, NewList})
            end,
            Res = ok;
        true ->
            Res = false
    end,
    {reply, Res, State};      


handle_call({register_pid, Pid, Login}, _From, #state{users_pid_tableId=TableId} = State) ->

    case ets:lookup(TableId, Login) of
        [] ->
            %io:format("~p register new user: ~p:~p\n", [self(), Login, Pid]),
            ets:insert(TableId, {Login, [Pid]});
        [{Key, List}] ->
            NewList = lists:append([List, [Pid]]),
            %io:format("~p add user pid: ~p:~p to ~p, all pid ~p\n", [self(), Key, Pid, List, NewList]),
            ets:insert(TableId, {Key, NewList})
    end,
    {reply, ok, State};

handle_call({unregister_pid, Pid, Login}, _From, #state{users_pid_tableId=TableId} = State) ->
    case ets:lookup(TableId, Login) of
        [] ->
           true;
        [{Key, List}] ->
            NewList = lists:delete(Pid, List),
            %io:format("~p remove user: ~p:~p Remaining ~p\n", [self(), Key, Pid, NewList]),
            ets:insert(TableId, {Key, NewList})
    end,
    {reply, ok, State};

handle_call({get_pids, Login}, _From, #state{users_pid_tableId=TableId, reg_users_data_tableId=TableHost, main_user_data=MUTid} = State) ->
    %находим домены пользователя
    Key = binary_to_atom(Login, utf8),
    %io:format("~p finded user: ~p login ~p\n", [self(), ets:lookup(TableHost, Key), Key]),
    case ets:lookup(TableHost, Key) of
        [] -> 
            All = [];
        [{_, MLogin}] ->
            %io:format("~p main data: ~p\n", [self(), ets:lookup(MUTid, MLogin)]),
            case ets:lookup(MUTid, MLogin) of
                [] -> 
                    All = [];
                [{_, Hosts}] ->
                    %io:format("~p finded domain: ~p login ~p\n", [self(), Hosts, MLogin]),
                    All = get_pids_by_hosts(Hosts, Login, TableId)
            end
    end,
    %io:format("~p finded user pids: ~p\n", [self(), All]),
    {reply, All, State}.
   

get_pids_by_hosts(Hosts, Login, TableId) ->
    get_pids_by_hosts(Hosts, [], Login, TableId).

get_pids_by_hosts([], All, _, _) ->
    All;
get_pids_by_hosts(Hosts, All, Login, TableId) ->
    [Host|NewHosts] = Hosts,
    Login_u = tcp_lib:get_login([Host, "_", Login]),
    %io:format("~p finded user pids: ~p login ~p All ~p\n", [self(), ets:lookup(TableId, Login_u), Login_u, All]),
    case ets:lookup(TableId, Login_u) of
        [] ->
            NewAll = lists:append([All, []]);
        [{_, List}] ->
            NewAll = lists:append([All, List])
    end,
    get_pids_by_hosts(NewHosts, NewAll, Login, TableId).

 
%%-------------------------------------------------------------------------
%% @spec (Msg, State) ->{noreply, State}          |
%%                      {noreply, State, Timeout} |
%%                      {stop, Reason, State}
%% @doc Callback for asyncrous server calls.  If `{stop, ...}' tuple
%%      is returned, the server is stopped and `terminate/2' is called.
%% @end
%% @private
%%-------------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.
 
%%-------------------------------------------------------------------------
%% @spec (Msg, State) ->{noreply, State}          |
%%                      {noreply, State, Timeout} |
%%                      {stop, Reason, State}
%% @doc Callback for messages sent directly to server's mailbox.
%%      If `{stop, ...}' tuple is returned, the server is stopped and
%%      `terminate/2' is called.
%% @end
%% @private
%%-------------------------------------------------------------------------
handle_info({inet_async, ListSock, Ref, {ok, CliSocket}},
            #state{listener=ListSock, acceptor=Ref, module=Module} = State) ->
    try
        case set_sockopt(ListSock, CliSocket) of
	        ok             
			  -> ok;
	        {error, Reason} 
			  -> exit({set_sockopt, Reason})
        end,
 
        %% New client connected - spawn a new process 
		%io:format("~p Conn info: ~p\n", [self(), inet:peername(CliSocket)]),
		{ok, GET_data} = gen_tcp:recv(CliSocket, 0),

        case tcp_lib:is_post_req(GET_data) of
            get -> 
        		case re:run(GET_data, "Origin\:\shttp\:\/\/(.*)\r\n",[global,{capture,[1],list}]) of
        	        {match, S_name} -> 
        				%io:format("~p matched: ~p\n", [self(), S_name]),
        				true;
        	        nomatch ->  
        				{ok, {Address, Port}} = inet:peername(CliSocket),
        				{ok,{hostent,Hostname,_,_,_,_}} = inet:gethostbyaddr(Address),
        				S_name = string:to_lower(Hostname)
        	    end;
            post ->
                S_name ="post_supervisor"
        end,
        {ok, Pid} = tcp_server_app:start_domain_supervisor(tcp_lib:convert_to_atom(S_name)),
		
        gen_tcp:controlling_process(CliSocket, Pid),
        %% Instruct the new FSM that it owns the socket.
        Module:set_socket(Pid, CliSocket, GET_data, S_name),
        %% Signal the network driver that we are ready to accept another connection
        case prim_inet:async_accept(ListSock, -1) of
	        {ok,    NewRef} 
			  -> ok;
	        {error, NewRef} 
			  -> exit({async_accept, inet:format_error(NewRef)})
        end,
 
        {noreply, State#state{acceptor=NewRef}}
    catch exit:Why ->
        error_logger:error_msg("Error in async accept: ~p.\n", [Why]),
        {stop, Why, State}
    end;
 
handle_info({inet_async, ListSock, Ref, Error}, #state{listener=ListSock, acceptor=Ref} = State) ->
    error_logger:error_msg("Error in socket acceptor: ~p.\n", [Error]),
    {stop, Error, State};
 
handle_info(_Info, State) ->
    %io:format("unhandle req ~p", Info),
    {noreply, State}.
 
%%-------------------------------------------------------------------------
%% @spec (Reason, State) -> any
%% @doc  Callback executed on server shutdown. It is only invoked if
%%       `process_flag(trap_exit, true)' is set by the server process.
%%       The return value is ignored.
%% @end
%% @private
%%-------------------------------------------------------------------------
terminate(_Reason, State) ->
    gen_tcp:close(State#state.listener),
    ok.
 
%%-------------------------------------------------------------------------
%% @spec (OldVsn, State, Extra) -> {ok, NewState}
%% @doc  Convert process state when code is changed.
%% @end
%% @private
%%-------------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
 
%%%------------------------------------------------------------------------
%%% Internal functions
%%%------------------------------------------------------------------------
 
%% Taken from prim_inet.  We are merely copying some socket options from the
%% listening socket to the new client socket.
set_sockopt(ListSock, CliSocket) ->
    true = inet_db:register_socket(CliSocket, inet_tcp),
    case prim_inet:getopts(ListSock, [active, nodelay, keepalive, delay_send, priority, tos]) of
    {ok, Opts} ->
        case prim_inet:setopts(CliSocket, Opts) of
        ok    -> ok;
        Error -> gen_tcp:close(CliSocket), Error
        end;
    Error ->
        gen_tcp:close(CliSocket), Error
    end.

get_client_api_key() ->
    {{Year,Month,Day},{Hour,_,_}} = erlang:localtime_to_universaltime(erlang:localtime()),
    Time = [integer_to_list(Year),integer_to_list(Month),integer_to_list(Day),integer_to_list(Hour)],
    Str = lists:flatten([?API_SALT, Time]),
    Mac = crypto:hash(md5,  Str),
    base64:encode(Mac).

get_user_by_key(Key) ->
	Mong = mongoapi:new(hawk_statistics,<<"hawk">>),
    Mong:findOne(#users{key=Key}).
	
rergister_user(Key, Id) ->
    gen_server:call({global, ?MODULE}, {rergister_user, Key, Id}).

unregister_user(Key, Id) ->
    gen_server:call({global, ?MODULE}, {unregister_user, Key, Id}).

is_user_exists(Id) ->
    gen_server:call({global, ?MODULE}, {check_user, Id}).

is_user_domain_exists(Id, Domain) ->
    gen_server:call({global, ?MODULE}, {check_user_domain, Id, Domain}).    

add_user_pid(Pid, Login) when is_pid(Pid) ->
    gen_server:call({global, ?MODULE}, {register_pid, Pid, Login}).

delete_user_pid(Pid, Login) when is_pid(Pid) ->
     gen_server:call({global, ?MODULE}, {unregister_pid, Pid, Login}).

get_user_pids(Login) ->
     gen_server:call({global, ?MODULE}, {get_pids, Login}).

add_domain(Key, Domain, Login) ->
     gen_server:call({global, ?MODULE}, {add_domain, Key, Domain, Login}).     

del_domain(Key, Domain, Login) ->
     gen_server:call({global, ?MODULE}, {del_domain, Key, Domain, Login}).    
