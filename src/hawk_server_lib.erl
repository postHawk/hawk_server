%% @author Barulin Maxim <mbarulin@gmail.com>
%% @copyright 2016 Barulin Maxim
%% @version 0.0.3
%% @title hawk_server_lib
%% @doc Библиотечный модуль с набором необходимых функций.
%% @todo хорошо бы навести порядок и перенести всё в макросы

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

-spec convert_to_binary(Value :: any(), Acc :: binary()) -> binary().
%% @doc Конвертирует значение в бинарный формат
convert_to_binary([], Value) -> lists:reverse(Value);
convert_to_binary([H|T] = _Value, Acc) ->
	NewAcc = [convert_to_binary({conv, H})|Acc],
	convert_to_binary(T, NewAcc).

-spec convert_to_binary(Value :: any()) -> binary().
convert_to_binary({conv, Value}) when is_binary(Value) -> Value;
convert_to_binary({conv, Value}) when is_list(Value) -> list_to_binary(Value);
convert_to_binary({conv, Value}) when is_atom(Value) -> atom_to_binary(Value, 'utf8');
convert_to_binary({conv, Value}) when is_tuple(Value) -> term_to_binary(Value);
convert_to_binary(Value) when is_list(Value)-> convert_to_binary(Value, []).

-spec get_api_action(Str :: binary()) -> {ok, Qtype :: binary(), Json :: list()} | false.
%% @doc Возвращает запрашииваемое действие
get_api_action(Str) ->
	Json = jsx:decode(Str),
	case proplists:get_value(<<"hawk_action">>, Json) of
		undefined -> false;
		Qtype 	  -> {ok, Qtype, Json}
	end.

-spec is_post_req(Data :: binary()) -> post | get.
%% @doc Определяет тип запроса
is_post_req(Data) ->
	case re:run(Data, "^POST \/ HTTP\/1.1\r\n") of
        {match, _} -> post;
        nomatch    -> get
    end.

-spec send_message(true | false, Frame :: binary(), S :: tuple(), T :: module()) -> ok.
%% @doc Отправляет сообщение пользователю
send_message(true, Frame, S, T) -> send_message(mask(Frame), S, T);
send_message(false, Frame, S, T) -> send_message({ok, Frame}, S, T).

-spec send_message({ok, Frame :: binary()}, S :: tuple(), T :: module()) -> ok.
send_message({ok, Frame}, S, T) ->
	case T:send(S, Frame) of
		ok -> ok;
		{error, _Reason} -> T:close(S)
	end.

-spec send_message_to_pid(Pid :: pid(), J_data :: list()) -> list() | binary().
%% @doc Отправляет сообщение другому процессу
send_message_to_pid(Pid, J_data) ->
	case is_process_alive(Pid) of
		true ->
			Pid ! {new_message, J_data},
			hawk_server_lib:get_server_message(<<"send_message">>, false, ?OK, false);
		false ->
			hawk_server_lib:get_server_message(<<"send_message">>, ?ERROR_USER_NOT_ONLINE)
	end.

-spec mask(Data :: binary()) -> {ok, Frame :: binary()}.
%% @doc Кодирует сообщение в формат web socket
mask(Data) ->
	Len = size(Data),
	if 	
		(Len >= 126) and (Len =< 65535) -> Frame = <<1:1, 0:3, 1:4, 0:1, 126:7, Len:16, Data/binary>>;
		Len > 65536                     -> Frame = <<1:1, 0:3, 1:4, 0:1, 127:7, Len:64, Data/binary>>;
		true                            -> Frame = <<1:1, 0:3, 1:4, 0:1, Len:7, Data/binary>>
	end,
	{ok, Frame}.

-spec flatten(X :: list()) -> list().
%% @doc Конвертирует массив в "плоский"
flatten(X)               -> flatten(X,[]).
flatten([],Acc)          -> Acc;
flatten([[]|T],Acc)      -> flatten(T, Acc);
flatten([[_|_]=H|T],Acc) -> flatten(T, flatten(H,Acc));
flatten([H|T],Acc)       -> flatten(T,Acc++[H]) .

-spec parse_header(Headers :: binary()) -> list().
%% @doc Разбирвет http запрос
parse_header(Headers)->
	[HeadersL, Body] = binary:split(Headers, <<"\r\n\r\n">>),
	HList = binary:split(HeadersL, <<"\r\n">>, [global]),
	[format_headers(HList), {body, Body}].

-spec format_headers(Headers :: list()) -> list().
%% @doc разбирает http заголовки в proplist
format_headers(Headers) -> format_headers(Headers, []).
format_headers([], Acc) -> lists:reverse(Acc);
format_headers([H|T] = _Headers, Acc) ->
	[Name, TVal] = binary:split(H, <<": ">>),
	Val =
		if
			Name == <<"Host">>  ->
				[Host, Port]= binary:split(TVal, <<":">>),
				NAcc = [{<<"Port">>, Port}|Acc],
				Host;
			true -> NAcc = Acc, TVal
		end,
	format_headers(T, [{Name, Val}|NAcc]).

-spec list_is_empty(List :: list()) -> boolean().
%% @doc Проверяет массив на пустоту.
%% Массив из пустых массивами считается пустым
list_is_empty(List) when is_list(List) -> lists:all(fun(L) -> L==[] end, List).

-spec loop_lists(Fun :: function(), List :: list()) -> ok.
-spec loop_lists(Method :: function, Fun :: function(), List :: list()) -> ok.
%% @doc Итерирует заданный набор массивов
loop_lists(Fun, Lists)               -> loop_lists(foreach, Fun, Lists, []). % Use foreach by default.
%% @doc Итерирует заданный набор массивов
%% указанной функцией
loop_lists(Method, Fun, Lists)       -> loop_lists(Method, Fun, Lists, []). % User-selected list function.
loop_lists(_Method, Fun, [], Args)   -> Fun(Args);
loop_lists(Method, Fun, [H|T], Args) ->
	FunLoop = fun(Elem) -> loop_lists(Method, Fun, T, Args ++ [Elem]) end,
	apply(lists, Method, [FunLoop, H]).

-spec get_server_message(Action :: binary(), Error :: binary()) -> list() | binary().
-spec get_server_message(Action :: binary(), Error :: binary(), Result :: binary()) -> list() | binary().
-spec get_server_message(Action :: binary(), Error :: binary(), Result :: binary(), Encode :: boolean()) -> list() | binary().
%% @doc Формирует сообщение для ответа клиенту
get_server_message(Action, Error)                 -> get_server_message(Action, Error, <<"">>, true).
get_server_message(Action, Error, Result)         -> get_server_message(Action, Error, Result, true).
get_server_message(Action, Error, Result, Encode) ->
	R1 = lists:keyreplace(action, 1, ?SERVER_REPLY, {action, Action}),
	R2 = lists:keyreplace(error, 1, R1, {error, Error}),
	R3 = lists:keyreplace(result, 1, R2, {result, Result}),
	if
		Encode == true -> jsx:encode(R3);
		true -> R3
	end.

-spec delete_keys(Keys :: list(), List :: tuple()) -> tuple().
%% @doc удаляет заданные ключи
delete_keys([], List) ->
	List;

delete_keys([Key|T] = _Keys, List) ->
	New_l = proplists:delete(Key, List),
	delete_keys(T, New_l).