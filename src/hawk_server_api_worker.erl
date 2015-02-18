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
			
            case dets:lookup(reg_users_data, Id) of
                [] ->
                    dets:insert(reg_users_data, {Id, Login});
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
            dets:delete(reg_users_data, Id),
            Reply = ?OK
    end,
	
    gen_server:reply(From, Reply),
    {stop, normal, State};

handle_cast({From, {check_user_by_domain, Id, Domain}}, State) ->
    case dets:lookup(reg_users_data, Id) of
        [] ->
            Reply = {ok, false, no_id};
        [{_, Login}] ->
            case dets:lookup(main_user_data, Login) of
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
            case dets:lookup(main_user_data, Login) of
                [] -> 
                    true;
                [{_, Hosts, Ukey}] ->
                    dets:insert(main_user_data, {Login, [Domain|Hosts], Ukey})
            end,
            Res = ?OK;
        true ->
            Res = false
    end,

    gen_server:reply(From, Res),
    {stop, normal, State};

handle_cast({From, {del_domain, Key, Domain, Login}}, State) ->
    Kur_key = get_client_api_key(),
    if 
        Key == Kur_key ->
            case dets:lookup(main_user_data, Login) of
                [] -> 
                    true;
                [{_, Hosts, Ukey}] ->
                    NewList = lists:delete(Domain, Hosts),
                    dets:insert(main_user_data, {Login, NewList, Ukey})
            end,
            Res = ?OK;
        true ->
            Res = false
    end,
	
    gen_server:reply(From, Res),
    {stop, normal, State};

handle_cast({From, {get_pids, Login, Domains}}, State) ->
    gen_server:reply(From, get_users_pids(Login, Domains)),
    {stop, normal, State};

handle_cast({From, {add_in_groups, Key, Login, Groups, Domains}}, State) ->
    {ok, User} = get_user_by_key(Key),
    case User of
        [] ->
            Reply = ?ERROR_INVALID_API_KEY;
        _ ->
			case check_user_domains(bson:lookup(domain, User), Domains) of
				true ->
		            case dets:lookup(reg_users_data, Login) of
		                [] ->
		                    Reply = ?ERROR_USER_NOT_REGISTER;
		                _ ->
		                    add_user_to_group(Login, Groups, Domains) ,
		                    Reply = ?OK
		            end;
				false ->
					 Reply = ?ERROR_DOMAIN_NOT_REGISTER
			end
    end,
    
    gen_server:reply(From, Reply),
    {stop, normal, State};

handle_cast({From, {remove_from_group, Key, Login, Groups, Domains}}, State) ->
    {ok, User} = get_user_by_key(Key),
    case User of
        [] ->
            Reply = ?ERROR_INVALID_API_KEY;
        _ ->
			case check_user_domains(bson:lookup(domain, User), Domains) of
				true ->
		            case dets:lookup(reg_users_data, Login) of
		                [] ->
		                    Reply = ?ERROR_USER_NOT_REGISTER;
		                _ ->
		                    remove_user_from_group(Login, Groups, Domains),
		                    Reply = ?OK
		            end;
				false ->
					 Reply = ?ERROR_DOMAIN_NOT_REGISTER
			end
    end,
    
    gen_server:reply(From, Reply),
    {stop, normal, State};

handle_cast({From, {get_by_group, Key, Groups, Domains}}, State) ->
    {ok, User} = get_user_by_key(Key),
    case User of
        [] ->
            Reply = ?ERROR_INVALID_API_KEY;
        _ ->
            Reply = get_users_by_groups(Groups, Domains)
    end,
    
    gen_server:reply(From, Reply),
    {stop, normal, State};

handle_cast({From, {check_user_domains, Domains, Login}}, State) ->
    {ok, User} = ?get_user_by_login(Login),
    case User of
        [] ->
            Reply = ?ERROR_INVALID_LOGIN_DATA;
        _ ->
            Reply = check_user_domains(bson:lookup(domain, User), Domains)
    end,
    
    gen_server:reply(From, Reply),
    {stop, normal, State};

handle_cast({From, {get_user_groups, Key, Login, Domains}}, State) ->
    {ok, User} = get_user_by_key(Key),
    case User of
        [] ->
            Reply = ?ERROR_INVALID_API_KEY;
        _ ->
            Reply = get_user_groups(Domains, Login)
    end,
    
    gen_server:reply(From, Reply),
    {stop, normal, State}.

%======================================================================
add_user_to_group(Login, Groups, Domains) ->
	lists:foreach(fun(Gr)->
		lists:foreach(fun(Dom)->
	        case dets:lookup(groups_to_user, {Gr, Dom}) of
	            [] ->
	                dets:insert(groups_to_user, {{Gr, Dom}, [Login]});
	            [{_, Users}] ->
	                case lists:member(Login, Users) of
	                    false ->
	                        dets:insert(groups_to_user, {{Gr, Dom}, [Login|Users]});
	                    true ->
	                        true
	                end
	        end,
			add_group_to_user(Login, Dom, Gr)
		end, Domains)
    end, Groups).

add_group_to_user(Login, Dom, Gr) ->
	case dets:lookup(user_to_groups, {Login, Dom}) of
		[] -> dets:insert(user_to_groups, {{Login, Dom}, [Gr]});
		[{_, Grps}] -> 
			case lists:member(Gr, Grps) of
                false ->
                    dets:insert(user_to_groups, {{Login, Dom}, [Gr|Grps]});
                true ->
                    true
            end
	end.

remove_user_from_group(Login, Groups, Domains) ->
	lists:foreach(fun(Gr)->
		lists:foreach(fun(Dom)->
	        case dets:lookup(groups_to_user, {Gr, Dom}) of
	            [] ->
	            	true;
	            [{_, Users}] ->
	            	dets:insert(groups_to_user, {{Gr, Dom}, lists:delete(Login, Users)})
	        end,
			remove_group_to_user(Login, Dom, Gr)		  
		end, Domains)
    end, Groups).

remove_group_to_user(Login, Dom, Gr) ->
	case dets:lookup(user_to_groups, {Login, Dom}) of
		[] -> true;
		[{_, Grps}] -> dets:insert(user_to_groups, {{Login, Dom}, lists:delete(Gr, Grps)})
	end.

get_users_pids(Logins, Domains) when is_list(Logins), is_list(Domains) ->
	Fun = fun(Login) ->
	    case dets:lookup(reg_users_data, Login) of
	        [] -> [];
             _ -> get_pids_by_hosts(Domains, Login)
	    end
	end,
	lists:flatten(lists:map(Fun, Logins));
 
get_users_pids(Login, Domain)  ->
    get_users_pids([Login], [Domain]).

get_pids_by_hosts(Hosts, Login) ->
	Fun = fun(Host) -> 
		    case gproc:lookup_pids({p, l, {Host, Login}}) of
				undefined -> [];
		        Pid 	  -> Pid
		    end
		end,
    lists:flatten(lists:map(Fun, Hosts)).

get_users_by_groups(Groups, Domains) ->
	Fun = fun(Gr) -> 
			get_users_in_group(Gr, Domains)
		end,
    List = lists:map(Fun, Groups),
	
	case hawk_server_lib:list_is_empty(List) of
		true -> [];
		false -> List
	end.

get_users_in_group(Gr, Domains) ->
	FunD = fun(Dom) ->
			case dets:lookup(groups_to_user, {Gr, Dom}) of
		        [] ->
		           [];
		        [{_, Users}] ->
					get_users_records(Users, Gr, Dom)
		    end		   
		end,
	case lists:map(FunD, Domains) of 
		[[]] -> [];
		R -> lists:flatmap(fun(X)->X end, R)
	end.

get_users_records(Users, Gr, Dom) ->
	Fun = fun(U) -> 
			[{group, Gr}, {user, U}, {online, is_user_online(U, Dom)}]
		end,
    lists:map(Fun, Users).

 is_user_online(U, Dom) ->
	case get_users_pids(U, Dom) of
		[] ->
			false;
		_ ->
			true
	end.

check_user_domains({UDomains}, Domains) when is_list(Domains)->
	case string:str(lists:sort(UDomains), lists:sort(Domains)) of
		0 -> false;
		_ -> true
	end;

check_user_domains(_UDomains, _Domains) ->
	false.

get_user_groups(Domains, Login) ->
	Fun = fun(Host) -> 
		    case dets:lookup(user_to_groups, {Login, Host}) of
				[] -> [];
				[{_, Grps}] -> Grps
			end
		end,
    lists:map(Fun, Domains).
	
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

	