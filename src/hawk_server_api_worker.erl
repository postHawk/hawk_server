%% @author Maximilian
%% @doc @todo Add description to api_controller.


-module(hawk_server_api_worker).
-behaviour(gen_server).
-include("env.hrl").
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
            Reply = ?get_server_message(<<"register_user">>, ?ERROR_INVALID_API_KEY);
        _ ->
			{Login} = bson:lookup(login, User),
			
            case dets:lookup(reg_users_data, Id) of
                [] ->
                    dets:insert(reg_users_data, {Id, Login});
                _ ->
                    true
            end,
            
            Reply = ?get_server_message(<<"register_user">>, false, ?OK)
    end,
	gen_server:reply(From, Reply),
    {stop, normal, State};

handle_cast({From, {unregister_user, Key, Id}}, State) ->
    {ok, User} = get_user_by_key(Key),
   
    case User of
        false ->
            Reply = ?get_server_message(<<"unregister_user">>, ?ERROR_INVALID_API_KEY);
        _ ->
            dets:delete(reg_users_data, Id),
            Reply = ?get_server_message(<<"unregister_user">>, false, ?OK)
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
            Res = ?get_server_message(<<"add_domain">>, false, ?OK);
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
            Res = ?get_server_message(<<"del_domain">>, false, ?OK);
        true ->
            Res = false
    end,
	
    gen_server:reply(From, Res),
    {stop, normal, State};

handle_cast({From, {get_pids, Login, Domains}}, State) ->
    gen_server:reply(From, get_users_pids(Login, Domains)),
    {stop, normal, State};

handle_cast({From, {add_in_groups, Key, Login, Groups, Domains, Restriction}}, State) ->
    {ok, User} = get_user_by_key(Key),
    Reply = case User of
        [] ->
            ?get_server_message(<<"add_in_groups">>, ?ERROR_INVALID_API_KEY);
        _ ->
			case check_user_domains(bson:lookup(domain, User), Domains) of
				true ->
		            case dets:lookup(reg_users_data, Login) of
		                [] ->
							?get_server_message(<<"add_in_groups">>, ?ERROR_USER_NOT_REGISTER);
		                _ ->
		                    add_user_to_group(Login, Groups, Domains, bson:lookup(login, User), Restriction) ,
							?get_server_message(<<"add_in_groups">>, false, ?OK)
		            end;
				false ->
					?get_server_message(<<"add_in_groups">>, ?ERROR_DOMAIN_NOT_REGISTER)
			end
    end,
    
    gen_server:reply(From, Reply),
    {stop, normal, State};

handle_cast({From, {remove_from_group, Key, Login, Groups, Domains, Restriction}}, State) ->
    {ok, User} = get_user_by_key(Key),
    case User of
        [] ->
            Reply = ?get_server_message(<<"remove_from_group">>, ?ERROR_INVALID_API_KEY);
        _ ->
			case check_user_domains(bson:lookup(domain, User), Domains) of
				true ->
		            case dets:lookup(reg_users_data, Login) of
		                [] ->
		                    Reply = ?get_server_message(<<"remove_from_group">>, ?ERROR_USER_NOT_REGISTER);
		                _ ->
		                    remove_user_from_group(Login, Groups, Domains, bson:lookup(login, User), Restriction),
		                    Reply = ?get_server_message(<<"remove_from_group">>, false, ?OK)
		            end;
				false ->
					 Reply = ?get_server_message(<<"remove_from_group">>, ?ERROR_DOMAIN_NOT_REGISTER)
			end
    end,
    
    gen_server:reply(From, Reply),
    {stop, normal, State};

handle_cast({From, {get_by_group, Key, Groups, Domains}}, State) ->
    {ok, User} = get_user_by_key(Key),
    Reply = case User of
        [] ->
            ?get_server_message(<<"get_by_group">>, ?ERROR_INVALID_API_KEY);
        _ ->
            get_users_by_groups(Groups, Domains)
    end,

    gen_server:reply(From, Reply),
    {stop, normal, State};

handle_cast({From, {check_user_domains, Domains, Login}}, State) ->
    {ok, User} = ?get_user_by_login(Login),
    case User of
        [] ->
            Reply = ?get_server_message(<<"check_user">>, ?ERROR_INVALID_LOGIN_DATA);
        _ ->
            Reply = check_user_domains(bson:lookup(domain, User), Domains)
    end,
    
    gen_server:reply(From, Reply),
    {stop, normal, State};

handle_cast({From, {get_user_groups, Key, Login, Domains}}, State) ->
    {ok, User} = get_user_by_key(Key),
    case User of
        [] ->
            Reply = ?get_server_message(<<"get_user_groups">>, ?ERROR_INVALID_API_KEY);
        _ ->
            Reply = get_user_groups(Domains, Login)
    end,
    
    gen_server:reply(From, Reply),
    {stop, normal, State};

handle_cast({From, {add_groups, Key, Groups, Domains}}, State) ->
    {ok, User} = get_user_by_key(Key),
    case User of
        [] ->
            Reply = ?get_server_message(<<"add_groups">>, ?ERROR_INVALID_API_KEY);
        _ ->
            Reply = add_groups(Groups, Domains, bson:lookup(login, User))
    end,
    
    gen_server:reply(From, Reply),
    {stop, normal, State};

handle_cast({From, {remove_groups, Key, Groups, Domains}}, State) ->
    {ok, User} = get_user_by_key(Key),
    Reply = case User of
        [] ->
            ?get_server_message(<<"remove_groups">>, ?ERROR_INVALID_API_KEY);
        _ ->
            remove_groups(Groups, Domains, bson:lookup(login, User))
    end,
    
    gen_server:reply(From, Reply),
    {stop, normal, State};

handle_cast({From, {is_user_in_group, Login, Group, [Dom|_] = _Domains}}, State) ->
	Reply = case dets:lookup(user_to_groups, {Login, Dom}) of
        [] ->
            false;
        [{_, Grps}] -> 
			lists:member(Group, Grps)
    end,
    gen_server:reply(From, Reply),
    {stop, normal, State};

handle_cast({From, {get_group_list, Key, Access, Domains}}, State) ->
	{ok, User} = get_user_by_key(Key),
    Reply = case User of
        [] ->
            ?get_server_message(<<"get_group_list">>, ?ERROR_INVALID_API_KEY);
        _ ->
            get_group_list(Access, Domains, bson:lookup(login, User))
    end,
    
    gen_server:reply(From, Reply),
    {stop, normal, State}.


%======================================================================
add_user_to_group(Login, Groups, Domains, {MLogin}, Restriction) ->
	lists:foreach(fun(Gr)->
		lists:foreach(fun(Dom)->
	        case dets:lookup(groups_to_user, {Gr, Dom}) of
	            [] ->
					Access = ?DEFAULT_GROUP_ACCESS,
	                dets:insert(groups_to_user, {{Gr, Dom}, [Login], Access}),
					add_group_to_user(Login, Dom, Gr);
	            [{_, Users, Access}] ->
	                case lists:member(Login, Users) of
	                    false ->
							if 
								Restriction == ?GROUP_ACCESS_ALL orelse 
									(Restriction == ?GROUP_ACCESS_PUBLIC andalso Access == ?GROUP_ACCESS_PUBLIC) ->
	                        		dets:insert(groups_to_user, {{Gr, Dom}, [Login|Users], Access}),
									add_group_to_user(Login, Dom, Gr);
								true -> 
								  	true
							end;
	                    true ->
	                        true
	                end
	        end,
			
			%link group to main user
			case dets:lookup(created_groups, {MLogin, Dom}) of
	         	[] -> 
					dets:insert(created_groups, {{MLogin, Dom}, [{Gr, Access}]});
			 	[{_, Grps}] ->
					NGrps = [GName || {GName, _} <- Grps],
					case lists:member(Gr, NGrps) of
						true -> 
							true;
						false -> 
							if 
								Restriction == ?GROUP_ACCESS_ALL orelse 
									(Restriction == ?GROUP_ACCESS_PUBLIC andalso Access == ?GROUP_ACCESS_PUBLIC) ->
									dets:insert(created_groups, {{MLogin, Dom}, [{Gr, Access}|Grps]});
								true ->
									true
							end
					end
			end
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

remove_user_from_group(Login, Groups, Domains, {MLogin}, Restriction) ->
	lists:foreach(fun(Gr)->
		lists:foreach(fun(Dom)->
	        case dets:lookup(groups_to_user, {Gr, Dom}) of
	            [] ->
	            	true;
	            [{_, Users, Access}] ->
					if 
						Restriction == ?GROUP_ACCESS_ALL orelse 
							(Restriction == ?GROUP_ACCESS_PUBLIC andalso Access == ?GROUP_ACCESS_PUBLIC) ->
							case lists:delete(Login, Users) of
								[] ->
									%remove empty group
									dets:delete(groups_to_user, {Gr, Dom}),
									dets:delete(created_groups, {MLogin, Dom});
								U ->
					            	dets:insert(groups_to_user, {{Gr, Dom}, U, Access})
							end,
							remove_group_to_user(Login, Dom, Gr);
						true ->
							true
					end
	        end
			
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
			get_users_in_group(record, Gr, Domains)
		end,
    [List] = lists:map(Fun, Groups),

	case hawk_server_lib:list_is_empty(List) of
		true -> [];
		false -> [L || L <- List, L /= []]
	end.

get_users_in_group(Type, Gr, Domains) ->
	FunD = fun(Dom) ->
			case dets:lookup(groups_to_user, {Gr, Dom}) of
		        [] -> [];
		        [{_, Users, Access}] -> 
					case Type of
						record -> get_users_records(Users, Gr, Dom, Access);
						list -> Users
					end
		    end		   
		end,
	[List]  = lists:map(FunD, Domains),

	case hawk_server_lib:list_is_empty(List) of
		true -> [];
		false -> List
	end.

get_users_records(Users, Gr, Dom, Access) ->
	Fun = fun(U) -> 
			[{group, Gr}, {user, U}, {online, is_user_online(U, Dom)}, {access, Access}]
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

add_groups(Groups, Domains, {MLogin}) ->
	lists:foreach(fun(Gr)->
		Access = proplists:get_value(<<"access">>, Gr),
		Name = proplists:get_value(<<"name">>, Gr),
		lists:foreach(fun(Dom)->
			%add group or change access
			case dets:lookup(groups_to_user, {Name, Dom}) of
				[] -> dets:insert(groups_to_user, {{Name, Dom}, [], Access});
				[{Key, Users, _A}] -> 
					dets:insert(groups_to_user, {Key, Users, Access})
			end,
			%link group to main user
			case dets:lookup(created_groups, {MLogin, Dom}) of
	         	[] -> 
					dets:insert(created_groups, {{MLogin, Dom}, [{Name, Access}]});
			 	[{_, Grps}] -> 
					NewGrps = [{GName, Acc} || {GName, Acc} <- Grps, GName /= Name],
					dets:insert(created_groups, {{MLogin, Dom}, [{Name, Access}|NewGrps]})
			end
		end, Domains)
    end, Groups),
	?get_server_message(<<"add_groups">>, false, ?OK).

remove_groups(Groups, Domains, {MLogin}) ->
	lists:foreach(fun(Gr)->
		Users = lists:flatten(get_users_in_group(list, Gr, Domains)),
		%delete user group and group
		lists:foreach(fun(User) ->
			lists:foreach(fun(Dom)->
	            remove_group_to_user(User, Dom, Gr),
				dets:delete(groups_to_user, {Gr, Dom}),
				case dets:lookup(created_groups, {MLogin, Dom}) of
		         	[] -> 
						true;
				 	[{_, Grps}] ->
						NewGrps = [{Name, Acc} || {Name, Acc} <- Grps, Name /= Gr],
						dets:insert(created_groups, {{MLogin, Dom}, NewGrps})
				end
				
			end, Domains)
		end, Users),
		
		%send message that group delete
		To_data = [{from, <<"server">>}, {action, <<"group_removed">>}, {group, Gr}],
		lists:foreach(fun(Pid)->
			hawk_server_lib:send_message_to_pid(Pid, To_data)
		end, get_users_pids(Users, Domains))	
    end, Groups),
	?get_server_message(<<"remove_groups">>, false, ?OK).

get_group_list(Access, Domains, {MLogin}) ->
	
	Groups = lists:map(fun(Dom)->
		case dets:lookup(created_groups, {MLogin, Dom}) of
			[] -> [];
			[{_, Grps}] -> 
				case Access of
					?GROUP_ACCESS_ALL -> 
						[[{name, Name}, {access, Acc}] || {Name, Acc} <- Grps];
					_ ->
						[[{name, Name}, {access, Acc}] || {Name, Acc} <- Grps, Acc == Access]
				end
		end
	end, Domains), 
	
	hd(Groups).

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

	