-module(tcp_lib).
-compile(export_all).

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
    get_ancestors(whereis(Name)).

ancestors_from_dict([]) ->
    [];
ancestors_from_dict([{'$ancestors', Ancestors} | _Rest]) ->
    Ancestors;
ancestors_from_dict([_Head | Rest]) ->
    ancestors_from_dict(Rest).

get_pid_process(Name) ->
	A_n = convert_to_atom(Name),
	case whereis(A_n) of
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
			unregister(A_f)
	end,			
	register(A_t, P_from),
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

info(Pid) ->  
	Spec = [registered_name],
 	case process_info(Pid, Spec) of
    	undefined ->
      		undefined;
    	Result ->
      		[{pid, Pid}|Result]
	end.

%можно немного оптимизировать так как is_pid вернёт тру, когда процес существует
get_uniq_user_login(Login) when is_pid(Login); is_atom(Login) ->
	case whereis(Login) of
		undefined ->
			Login;
		_ ->
			get_uniq_user_login(Login, 0)
	end;
get_uniq_user_login(Login) when is_list(Login); is_binary(Login) ->
	NewLogin = convert_to_atom(Login),
	case whereis(NewLogin) of
		undefined ->
			NewLogin;
		_ ->
			get_uniq_user_login(Login, 0)
	end.

get_uniq_user_login(Login, Counter) ->
	NewLoginStr = [Login, "_", Counter],
	NewLogin = convert_to_atom(NewLoginStr),
	case whereis(NewLogin) of
		undefined ->
			NewLogin;
		_ ->
			get_uniq_user_login(NewLoginStr, Counter+1)
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

get_json_from_post(POST_data) ->
	%io:format("~p post messge ~p\n", [self(), POST_data]),	

	case re:run(POST_data, "\"[^\r\n]+\"\r\n\r\n([^\r\n]+)\r\n", [global,{capture,[1],list}]) of
        {match, Matched} -> 
        	Str = lists:flatten(Matched),
        	split_json_by_part(Str);
        nomatch ->  
   			false
   	end.
split_json_by_part(Str) ->
	case re:run(Str, "^\{([^{]+)\}", [global,{capture,[1],list}]) of
		{match, Qtype} ->
			{ok, Qtype, [[re:replace(Str, "^\{([^{]+)\}", "", [global, {return,list}])]]};
		nomatch ->  
				false
	end.

is_post_req(Data) ->
	case re:run(Data, "^POST \/ HTTP\/1.1\r\n") of
        {match, _} -> 
			true;
        nomatch ->  
			false
    end.