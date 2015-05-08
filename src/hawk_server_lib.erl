-module(hawk_server_lib).
-compile(export_all).
-include("env.hrl").
-include("mac.hrl").

get_ancestors(Pid) when is_pid(Pid) ->
    case erlang:process_info(Pid, dictionary) of
        {dictionary, D} ->
            ancestors_from_dict(D);
        _ ->
            []
    end;
get_ancestors(undefined) ->
    [];
get_ancestors(Name) when is_atom(Name) ->
    get_ancestors(global:whereis_name(Name)).

ancestors_from_dict([]) ->
    [];
ancestors_from_dict([{'$ancestors', Ancestors} | _Rest]) ->
    Ancestors;
ancestors_from_dict([_Head | Rest]) ->
    ancestors_from_dict(Rest).

get_pid_process(Name) ->
	A_n = convert_to_atom(Name),
	case global:whereis_name(A_n) of
		undefined ->
			{ok, false};
		Pid ->
			{ok, Pid}
	end.

rename_process(From, To) ->
	A_f = convert_to_atom(From),
	A_t = convert_to_atom(To),
	case get_pid_process(A_f) of
		{ok, P_from} ->
			global:unregister_name(A_f)
	end,			
	global:register_name(A_t, P_from),
	ok.

convert_to_atom(Value) when is_atom(Value) ->
	Value;
convert_to_atom(Value) when is_binary(Value) ->
	binary_to_atom(Value, utf8);
convert_to_atom(Value) when is_list(Value) ->
	Bin = list_to_binary(Value),
	convert_to_atom(Bin);
convert_to_atom(Value) when is_tuple (Value) ->
	R = io_lib:format("~p",[Value]),
	convert_to_atom(lists:flatten(R)).

convert_to_binary([], Value) ->
	lists:reverse(Value);
convert_to_binary([H|T] = _Value, Acc) ->
	NewAcc = [convert_to_binary({conv, H})|Acc],
	convert_to_binary(T, NewAcc).

convert_to_binary({conv, Value}) when is_binary(Value) ->
	Value;
convert_to_binary({conv, Value}) when is_list(Value) ->
	list_to_binary(Value);
convert_to_binary({conv, Value}) when is_atom(Value) ->
	atom_to_binary(Value, 'utf8');
convert_to_binary({conv, Value}) when is_tuple(Value) ->
	term_to_binary(Value);
convert_to_binary(Value) when is_list(Value)->
	convert_to_binary(Value, []).

info(Pid) ->  
	Spec = [registered_name],
 	case process_info(Pid, Spec) of
    	undefined ->
      		undefined;
    	Result ->
      		[{pid, Pid}|Result]
	end.

get_uniq_user_login(Login) when is_pid(Login); is_atom(Login) ->
	case global:whereis_name(Login) of
		undefined ->
			Login;
		_ ->
			get_uniq_user_login(Login, 1)
	end;
get_uniq_user_login(Login) when is_list(Login); is_binary(Login) ->
	NewLogin = convert_to_atom(Login),
	case global:whereis_name(NewLogin) of
		undefined ->
			NewLogin;
		_ ->
			get_uniq_user_login(Login, 1)
	end.

get_uniq_user_login(Login, Counter) ->
	NewLoginStr = [Login, "_u", integer_to_list(Counter)],
	NewLogin = convert_to_atom(NewLoginStr),
	case global:whereis_name(NewLogin) of
		undefined ->
			NewLogin;
		_ ->
			get_uniq_user_login(Login, Counter+1)
	end. 

get_login(Login) ->
	StrLogin = list_to_binary(Login),
	try binary_to_atom(StrLogin, utf8) of
		L -> L
	catch  _:_ -> 
		try	binary_to_existing_atom(StrLogin, utf8) of
			L -> L
		catch  _:_ -> 
			false
		end
	end.

split_json_by_part(Str) ->
	Json = jsx:decode(Str),
	case proplists:get_value(<<"hawk_action">>, Json) of
		undefined -> false;
		Qtype 	  -> {ok, Qtype, Json}
	end.

is_post_req(Data) ->
	case re:run(Data, "^POST \/ HTTP\/1.1\r\n") of
        {match, _} -> 
			post;
        nomatch ->  
			get
    end.

send_message({ok, Frame}, S, T) ->
	case T:send(S, Frame) of
		ok ->
			ok;
		{error, _Reason} -> 
			T:close(S)
	end.

send_message_to_pid(Pid, J_data) ->
	case is_process_alive(Pid) of
		true ->
			Pid ! {new_message, J_data},
			?get_server_message(<<"send_message">>, false, ?OK);
		false ->
			?get_server_message(<<"send_message">>, ?ERROR_USER_NOT_ONLINE)
	end.

pid_2_name(Pid) ->
    case ets:lookup(global_pid_names, Pid) of
		[{Pid, Name}] -> 
		    if node(Pid) == node() ->
		    	case is_process_alive(Pid) of
					true -> Name;
					false -> undefined
		    	end;
		    true ->
		    	Name
		    end;
		[] -> undefined
    end.

unique_list(List) ->
    lists:reverse(
        lists:foldl(
        	fun(X,Acc) ->
                accumulate_unless(lists:member(X,Acc),X,Acc)
         	end,
        [],
        List)).

accumulate_unless(true, _X, Acc) ->
    Acc;
accumulate_unless(false, X, Acc) ->
    [X|Acc].

remove_dups([])    -> [];
   remove_dups([H|T]) -> [H | [X || X <- remove_dups(T), X /= H]].

flatten(X)               -> flatten(X,[]).

flatten([],Acc)          -> Acc;
flatten([[]|T],Acc)      -> flatten(T, Acc);
flatten([[_|_]=H|T],Acc) -> flatten(T, flatten(H,Acc));
flatten([H|T],Acc)       -> flatten(T,Acc++[H]) .

parse_header(Headers)->
	[HeadersL, Body] = binary:split(Headers, <<"\r\n\r\n">>),
	HList = binary:split(HeadersL, <<"\r\n">>, [global]),
	[format_headers(HList), {body, Body}].

format_headers(Headers) ->
	format_headers(Headers, []).

format_headers([], Acc) ->
	lists:reverse(Acc);
format_headers([H|T] = _Headers, Acc) ->
	[Name, Val] = binary:split(H, <<": ">>),
	format_headers(T, [{Name, Val}|Acc]).
	
list_is_empty(List) ->
	lists:all(fun(L) -> L==[] end, List).