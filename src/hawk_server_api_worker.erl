%% @author Maximilian
%% @doc @todo Add description to api_controller.


-module(hawk_server_api_worker).
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

-record(users_in_group, {group, user, online}).
-record(users_in_group_for_message, {group, users}).

%% ====================================================================
%% Behavioural functions 
%% ====================================================================

start_link()->
    gen_server:start_link(?MODULE, [], []).

init([]) ->
	{ok, #state{}}.

%% ====================================================================
%% Internal functions
%% ====================================================================

handle_cast({From, {register_user, Key, Id}}, State) ->
    {ok, User} = get_user_by_key(Key),

    case User of
        false ->
            Reply = false;
        _ ->
			{Login} = bson:lookup(login, User),
			
            case ets:lookup(reg_users_data, Id) of
                [] ->
                    ets:insert(reg_users_data, {Id, Login});
                _ ->
                    true
            end,
            
            Reply = ?OK
    end,
	gen_server:reply(From, Reply),
    {stop, normal, State};

handle_cast({From, {unregister_user, Key, Id}}, State) ->
    {ok, User} = get_user_by_key(Key),
   
    case User of
        false ->
            Reply = false;
        _ ->
            ets:delete(reg_users_data, Id),
            Reply = ?OK
    end,
	
    gen_server:reply(From, Reply),
    {stop, normal, State};

handle_cast({From, {check_user_by_domain, Id, Domain}}, State) ->
    case ets:lookup(reg_users_data, Id) of
        [] ->
            Reply = {ok, false, no_id};
        [{_, Login}] ->
            case ets:lookup(main_user_data, Login) of
                [] -> 
                    Reply = {ok, false, no_login};
                [{_, Hosts, _}] ->
                    case lists:member(Domain, Hosts) of
                        true ->
                            Reply = {ok, true};
                        false ->
                            Reply = {ok, false, no_domain}
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
                [{_, Hosts, Ukey}] ->
                    ets:insert(main_user_data, {Login, [Domain|Hosts], Ukey})
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
                [{_, Hosts, Ukey}] ->
                    NewList = lists:delete(Domain, Hosts),
                    ets:insert(main_user_data, {Login, NewList, Ukey})
            end,
            Res = ok;
        true ->
            Res = false
    end,
	
    gen_server:reply(From, Res),
    {stop, normal, State};

handle_cast({From, {get_pids, Login}}, State) ->
    gen_server:reply(From, get_users_pids(Login)),
    {stop, normal, State};

handle_cast({From, {add_in_groups, Key, Login, Groups}}, State) ->
    {ok, User} = get_user_by_key(Key),
    case User of
        [] ->
            Reply = ?ERROR_INVALID_API_KEY;
        _ ->
            case ets:lookup(reg_users_data, Login) of
                [] ->
                    Reply = ?ERROR_USER_NOT_REGISTER;
                _ ->
                    add_user_to_group(bson:lookup(login, User), Login, Groups) ,
                    Reply = ?OK
            end
    end,
    
    gen_server:reply(From, Reply),
    {stop, normal, State};

handle_cast({From, {remove_from_group, Key, Login, Groups}}, State) ->
    {ok, User} = get_user_by_key(Key),
    case User of
        [] ->
            Reply = ?ERROR_INVALID_API_KEY;
        _ ->
            case ets:lookup(reg_users_data, Login) of
                [] ->
                    Reply = ?ERROR_USER_NOT_REGISTER;
                _ ->
                    remove_user_from_group(bson:lookup(login, User), Login, Groups),
                    Reply = ?OK
            end
    end,
    
    gen_server:reply(From, Reply),
    {stop, normal, State};

handle_cast({From, {get_by_group, Key, Groups}}, State) ->
    {ok, User} = get_user_by_key(Key),
    case User of
        [] ->
            Reply = ?ERROR_INVALID_API_KEY;
        _ ->
			ULogin = bson:lookup(login, User),
            Reply = get_users_by_groups(Groups, ULogin)
    end,
    
    gen_server:reply(From, Reply),
    {stop, normal, State};

handle_cast({From, {get_by_group_for_message, Key, Groups}}, State) ->
    {ok, User} = get_user_by_key(Key),
    case User of
        [] ->
            Reply = ?ERROR_INVALID_API_KEY;
        _ ->
			ULogin = bson:lookup(login, User),
            Reply = get_users_by_groups_for_message(Groups, ULogin)
    end,

    gen_server:reply(From, Reply),
    {stop, normal, State}.

add_user_to_group(MLogin, Login, Groups) ->
	lists:foreach(fun(Gr)->
        case ets:lookup(groups_to_user, {Gr, MLogin}) of
            [] ->
                ets:insert(groups_to_user, {{Gr, MLogin}, [Login]});
            [{_, Users}] ->
                case lists:member(Login, Users) of
                    false ->
                        ets:insert(groups_to_user, {{Gr, MLogin}, [Login|Users]});
                    true ->
                        true
                end
        end
    end, Groups).

remove_user_from_group(MLogin, Login, Groups) ->
	lists:foreach(fun(Gr)->
        case ets:lookup(groups_to_user, {Gr, MLogin}) of
            [] ->
               true;
            [{_, Users}] ->
                NewUsers = lists:delete(Login, Users),
                ets:insert(groups_to_user, {{Gr, MLogin}, NewUsers})
        end
    end, Groups).

get_users_pids(Logins) when is_list(Logins) ->
    Fun = fun(Login) ->
	    case ets:lookup(reg_users_data, Login) of
	        [] 			  -> [];
	        [{_, MLogin}] ->
	            case ets:lookup(main_user_data, MLogin) of
	                [] 				-> [];
	                [{_, Hosts, _}] -> get_pids_by_hosts(Hosts, Login)
	            end
	    end
	end,
	lists:flatten(lists:map(Fun, Logins));

get_users_pids(Login)  ->
    get_users_pids([Login]).

get_pids_by_hosts(Hosts, Login) ->
	Fun = fun(Host) -> 
		    case gproc:lookup_pids({p, l, {Host, Login}}) of
				undefined -> [];
		        Pid 	  -> Pid
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
    lists:map(Fun, Groups).

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
			[{group, Gr}, {user, U}, {online, is_user_online(U)}]
		end,
    lists:map(Fun, Users).

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
	?get_user_by_key(Key).

	