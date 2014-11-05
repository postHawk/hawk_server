%% @author Maximilian
%% @doc @todo Add description to api_controller.


-module(api_worker).
-behaviour(gen_server).
-include("mac.hrl").

-export([init/1, start_link/0, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([]).

-record(state, {
                listener,       % Listening socket
                acceptor,       % Asynchronous acceptor's internal reference
                module          % FSM handling module
               }).

-record(users, {
    login,
    password,
    email,
    activation_code,
    key,
    domain
}).

-record(users_in_group, {group, user, online}).
-record(users_in_group_for_message, {group, users}).


%% ====================================================================
%% Behavioural functions 
%% ====================================================================


start_link()->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
	unregister(?MODULE),
	mongoapi:recinfo(#users{}, record_info(fields, users)),
	{ok, #state{}}.

%% ====================================================================
%% Internal functions
%% ====================================================================

handle_cast({From, {register_user, Key, Id}}, State) ->
    {ok, User} = get_user_by_key(Key),
    case User of
        [] ->
            Reply = false;
        _ ->
			#users{domain=Host, login=Login, key=ApiKey} = User,
            %регистрируем данные учётки пользователя сайта
            case ets:lookup(main_user_data, Login) of
                [] ->
                    {array, List} = Host,
                    ets:insert(main_user_data, {Login, List, ApiKey});
                _ ->
                    true
            end,

            %регистрируем пользователя для обеспечения возможности пересылки сообщений
            %и связываем его с конкретной учёткой на сайте через логин
            Key_t = binary_to_atom(Id, utf8) ,
            case ets:lookup(reg_users_data, Key_t) of
                [] ->
                    ets:insert(reg_users_data, {Key_t, Login});
                _ ->
                    true
            end,
            
            Reply = "ok"
    end,
	gen_server:reply(From, Reply),
    {stop, normal, State};

handle_cast({From, {unregister_user, Key, Id}}, State) ->
    {ok, User} = get_user_by_key(Key),
   
    case User of
        [] ->
            Reply = false;
        _ ->
            Key_t = binary_to_atom(Id, utf8) ,

            case ets:lookup(reg_users_data, Key_t) of
                [] ->
                   true;
                _ ->
                    ets:delete(reg_users_data, Key_t)
            end,
            
            Reply = ok
    end,
    gen_server:reply(From, Reply),
    {stop, normal, State};

handle_cast({From, {check_user, Id}}, State) ->
	case ets:lookup(reg_users_data, Id) of
        [] ->
            Reply = {ok, false};
        _ ->
            Reply = {ok, true}
    end,
	
	gen_server:reply(From, Reply),
    {stop, normal, State};

handle_cast({From, {check_user_domain, Id, Domain}}, State) ->
    case ets:lookup(reg_users_data, Id) of
        [] ->
            Reply = {ok, false};
        [{_, Login}] ->
            case ets:lookup(main_user_data, Login) of
                [] -> 
                    Reply = {ok, false};
                [{_, Hosts, _}] ->
                    case lists:member(Domain, Hosts) of
                        true ->
                            Reply = {ok, true};
                        false ->
                            Reply = {ok, false}
                    end
            end
    end,
	
	gen_server:reply(From, Reply),
    {stop, normal, State};

handle_cast({From, {add_domain, Key, Domain, Login}}, State) ->
    
    Kur_key = get_client_api_key(),
    if 
        Key == Kur_key ->
            case ets:lookup(main_user_data, Login) of
                [] -> 
                    true;
                [{_, Hosts, _}] ->
                    NewList = lists:append([Hosts, [Domain]]),
                    ets:insert(main_user_data, {Login, NewList})
            end,
            Res = ok;
        true ->
            Res = false
    end,

    gen_server:reply(From, Res),
    {stop, normal, State};

handle_cast({From, {del_domain, Key, Domain, Login}}, State) ->
    Kur_key = get_client_api_key(),
    if 
        Key == Kur_key ->
            case ets:lookup(main_user_data, Login) of
                [] -> 
                    true;
                [{_, Hosts, _}] ->
                    NewList = lists:delete(Domain, Hosts),
                    ets:insert(main_user_data, {Login, NewList})
            end,
            Res = ok;
        true ->
            Res = false
    end,
	
    gen_server:reply(From, Res),
    {stop, normal, State};

handle_cast({From, {register_pid, Pid, Login}}, State) ->

    case ets:lookup(users_pids, Login) of
        [] ->
            ets:insert(users_pids, {Login, [Pid]});
        [{Key, List}] ->
            NewList = lists:append([List, [Pid]]),
            ets:insert(users_pids, {Key, NewList})
    end,
	
    gen_server:reply(From, ok),
    {stop, normal, State};

handle_cast({From, {unregister_pid, Pid, Login}}, State) ->
    case ets:lookup(users_pids, Login) of
        [] ->
           true;
        [{Key, List}] ->
            NewList = lists:delete(Pid, List),
            ets:insert(users_pids, {Key, NewList})
    end,
	
    gen_server:reply(From, ok),
    {stop, normal, State};

handle_cast({From, {get_pids, Login}}, State) ->
    All = get_users_pids(Login),
	
    gen_server:reply(From, All),
    {stop, normal, State};

%ужас, нужно что-то с этим сделать
%оптимально перенести группы в монгу
handle_cast({From, {add_in_groups, Key, Login, Groups}}, State) ->
    {ok, User} = get_user_by_key(Key),
    case User of
        [] ->
            Reply = <<"invalid_api_key">>;
        _ ->
            Key_t = binary_to_atom(Login, utf8) ,
            case ets:lookup(reg_users_data, Key_t) of
                [] ->
                    Reply = <<"user_not_register">>;
                _ ->
                    #users{login=ULogin} = User,
                    lists:foreach(fun(Gr)->
                        case ets:lookup(groups_to_user, {Gr, ULogin}) of
                            [] ->
                                ets:insert(groups_to_user, {{Gr, ULogin}, [Login]});
                            [{_, Users}] ->
                                case lists:member(Login, Users) of
                                    false ->
                                        NewUsers = lists:append([Users, [Login]]),
                                        ets:insert(groups_to_user, {{Gr, ULogin}, NewUsers});
                                    true ->
                                        true
                                end
                        end
                    end, Groups),
                    Reply = <<"ok">>
            end
    end,
    
    gen_server:reply(From, Reply),
    {stop, normal, State};

handle_cast({From, {remove_from_group, Key, Login, Groups}}, State) ->
    {ok, User} = get_user_by_key(Key),
    case User of
        [] ->
            Reply = <<"invalid_api_key">>;
        _ ->
            Key_t = binary_to_atom(Login, utf8) ,
            case ets:lookup(reg_users_data, Key_t) of
                [] ->
                    Reply = <<"user_not_register">>;
                _ ->
                    #users{login=ULogin} = User,
                    lists:foreach(fun(Gr)->
                        %убеждаемся, что группа существует
                        case ets:lookup(groups_to_user, {Gr, ULogin}) of
                            [] ->
                               true;
                            [{_, Users}] ->
                                NewUsers = lists:delete(Login, Users),
                                ets:insert(groups_to_user, {{Gr, ULogin}, NewUsers})
                        end
                    end, Groups),
                    Reply = <<"ok">>
            end
    end,
    
    gen_server:reply(From, Reply),
    {stop, normal, State};

handle_cast({From, {get_by_group, Key, Groups}}, State) ->
    {ok, User} = get_user_by_key(Key),
    case User of
        [] ->
            Reply = <<"invalid_api_key">>;
        _ ->
            #users{login=ULogin} = User,
            Reply = get_users_by_groups(Groups, ULogin)
    end,
    
    gen_server:reply(From, Reply),
    {stop, normal, State};

handle_cast({From, {get_by_group_for_message, Key, Groups}}, State) ->
    {ok, User} = get_user_by_key(Key),
    case User of
        [] ->
            Reply = <<"invalid_api_key">>;
        _ ->
            #users{login=ULogin} = User,
            Reply = get_users_by_groups_for_message(Groups, ULogin)
    end,

    gen_server:reply(From, Reply),
    {stop, normal, State}.

get_users_pids(Logins) when is_list(Logins) ->
    Fun = fun(Login) ->
		    Key = binary_to_atom(Login, utf8),
		    case ets:lookup(reg_users_data, Key) of
		        [] -> 
		           [];
		        [{_, MLogin}] ->
		            case ets:lookup(main_user_data, MLogin) of
		                [] -> 
		                    [];
		                [{_, Hosts, _}] ->
		                    get_pids_by_hosts(Hosts, Login)
		            end
		    end
		end,
		lists:flatten(lists:map(Fun, Logins));

get_users_pids(Login)  ->
    get_users_pids([Login]).

get_pids_by_hosts(Hosts, Login) ->
	Fun = fun(Host) -> 
		    Login_u = tcp_lib:get_login([Host, "_", Login]),
		    case ets:lookup(users_pids, Login_u) of
		        [] ->
		            [];
		        [{_, List}] ->
		            List
		    end
		end,
    lists:flatten(lists:map(Fun, Hosts)).

get_users_by_groups(Groups, ULogin) ->
    Fun = fun(Gr) -> 
		    case ets:lookup(groups_to_user, {Gr, ULogin}) of
		        [] ->
		           [];
		        [{_, Users}] ->
					get_users_records(Users, Gr)
		    end
		end,
    lists:flatten(lists:map(Fun, Groups)).


get_users_by_groups_for_message(Groups, ULogin) ->
   Fun = fun(Gr) -> 
		    case ets:lookup(groups_to_user, {Gr, ULogin}) of
		        [] ->
		            [];
		        [{_, Users}] ->
		           [#users_in_group_for_message{group=Gr, users=Users}]
			end
		end,
    lists:flatten(lists:map(Fun, Groups)).

get_users_records(Users, Gr) ->
	Fun = fun(U) -> 
			[#users_in_group{group=Gr, user=U, online=is_user_online(U)}]
		end,
    lists:flatten(lists:map(Fun, Users)).

 is_user_online(U) ->
	case get_users_pids(U) of
		[] ->
			false;
		_ ->
			true
	end.

handle_info(_Data, State) ->
	{stop, normal, State}.

handle_call(_Msg, _From, State) ->
    {stop, normal, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

%=================API FUNCTION===========================================

get_client_api_key() ->
    {{Year,Month,Day},{Hour,_,_}} = erlang:localtime_to_universaltime(erlang:localtime()),
    Time = [integer_to_list(Year),integer_to_list(Month),integer_to_list(Day),integer_to_list(Hour)],
    Str = lists:flatten([?API_SALT, Time]),
    Mac = crypto:hash(md5,  Str),
    base64:encode(Mac).

get_user_by_key(Key) ->
	Mong = mongoapi:new(hawk_statistics,<<"hawk">>),
    Mong:findOne(#users{key=Key}).
	