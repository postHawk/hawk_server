%% @author Barulin Maxim <mbarulin@gmail.com>
%% @copyright 2016 Barulin Maxim
%% @version 0.0.3
%% @title hawk_server_api_worker
%% @doc Рабочий процесс, обрабатывающий запрос к апи. Вызывается и создаётся hawk_client_api_manager


-module(hawk_server_api_worker).
-behaviour(gen_server).
-include("env.hrl").
-include("mac.hrl").

-export([init/1, start_link/0, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([get_user_from_env/1]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([]).

-record(state, {listener, acceptor, module}).

%% ====================================================================
%% Behavioural functions 
%% ====================================================================
%% @doc Запуск модуля
start_link() ->
	gen_server:start_link(?MODULE, [], []).

%% @doc Инициализация модуля.
init([]) ->
	{ok, #state{}}.

-spec handle_cast({{pid(), reference()} | terminate, Params :: any()}, State :: #state{}) -> {noreply, State :: #state{}} | {stop, normal, State :: #state{}}.
%% @doc Обработчик запроса к апи
handle_cast({From, Params}, State) ->
	Key = element(2, Params),
	{ok, User} = get_user_by_key(Key),
	case User of
		false ->
			Reply = hawk_server_lib:get_server_message(
				atom_to_binary(element(1, Params), utf8),
				?ERROR_INVALID_API_KEY
			);
		_ -> Reply = api_action(Params, User)
	end,

	hawk_server_api_manager:restore_worker(self()),
	gen_server:reply(From, Reply),
	{noreply, State};

handle_cast(terminate, State) -> {stop, normal, State}.

-spec api_action(
	{add_domain, Key :: binary(), Domain :: binary(), Login :: binary()} |
	{del_domain, Key :: binary(), Domain :: binary(), Login :: binary()} |
	{check_user_by_domain, Key :: binary(), Id :: binary(), Domain :: binary()} |
	{check_user_domains, Key :: binary(), Domains :: [binary()], Login :: binary()} |
	{add_user, Key :: binary()} |
	{get_user, Key :: binary()} |
	{get_token, _Key :: binary(), Login :: binary(), Salt :: binary(), Domains :: [binary()]}, User :: #{}) -> binary() | list().

%% @doc Добавление домена пользователя
api_action({add_domain, Key, Domain, Login}, _User) ->
	%нельзя вставить вызов функции в условие
	CKey = get_client_api_key(),
	if
		Key == CKey -> add_user_domain(Login, Domain);
		true -> hawk_server_lib:get_server_message(<<"add_domain">>, ?ERROR_USER_NOT_REGISTER)
	end;

%% @doc Удаление домена пользователя
api_action({del_domain, Key, Domain, Login}, _User) ->
	CKey = get_client_api_key(),
	if
		Key == CKey ->
			remove_user_domain(Login, Domain),
			hawk_server_lib:get_server_message(<<"del_domain">>, false, ?OK);
		true ->
			false
	end;

%% @doc Проверяет полную регистрацию пользователя на домене
api_action({check_user_by_domain, _Key, _Id, Domain}, User) ->
	Mlogin = maps:get(<<"login">>, User),

	case dets:lookup(main_user_data, Mlogin) of
		[] ->
			{ok, false, no_login};
		[{_, Hosts, _}] ->
			case lists:member(Domain, Hosts) of
				true ->
					{ok, true};
				false ->
					{ok, false, no_domain}
			end
	end;

%% @doc Проверяет регистрацию домена в системе
api_action({check_user_domains, _Key, Domains, _Login}, User) ->
	check_user_domains(maps:get(<<"domain">>, User), Domains);

%% @doc Регистрация пользователя
api_action({add_user, _Key}, User) ->
	Login = maps:get (<<"login">>, User),
	U_hosts = maps:get(<<"domain">>, User),
	ApiKey = maps:get(<<"key">>, User),
	%обновялем пользователя в любом случае
	dets:insert(main_user_data, {Login, U_hosts, ApiKey}),
	{ok, Login};

%% @doc Возвращает пользователя
api_action({get_user, _Key}, User) ->
	{ok, User}.


-spec check_user_domains(UDomains :: [binary()], Domains :: [binary()]) -> boolean().
%% @doc Проверяет входит ли домен пользователя в список зарегистрированных
check_user_domains(UDomains, Domains) when is_list(Domains) and is_list(UDomains) ->
	case string:str(lists:sort(UDomains), lists:sort(Domains)) of
		0 -> false;
		_ -> true
	end;

check_user_domains(_UDomains, _Domains) -> false.

handle_info(_Data, State) -> {noreply, State}.
handle_call(_Msg, _From, State) -> {noreply, State}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_Reason, _State) -> ok.

%=================API FUNCTION===========================================

get_client_api_key() ->
	{{Year, Month, Day}, {Hour, _, _}} = erlang:localtime_to_universaltime(erlang:localtime()),
	Time = [integer_to_list(Year), integer_to_list(Month), integer_to_list(Day), integer_to_list(Hour)],
	Str = lists:flatten([?API_SALT, Time]),
	Mac = crypto:hash(md5, Str),
	base64:encode(Mac).

add_user_domain(Login, Domain)->
	case dets:lookup(main_user_data, Login) of
		[] ->
			true;
		[{_, Hosts, Ukey}] ->
			dets:insert(main_user_data, {Login, [Domain | Hosts], Ukey})
	end,
	hawk_server_lib:get_server_message(<<"add_domain">>, false, ?OK).

remove_user_domain(Login, Domain) ->
	case dets:lookup(main_user_data, Login) of
		[] ->
			true;
		[{_, Hosts, Ukey}] ->
			NewList = lists:delete(Domain, Hosts),
			dets:insert(main_user_data, {Login, NewList, Ukey})
	end.

get_user_by_key(Key) -> get_user_from_env({<<"key">>, Key}).

get_user_from_env(Criteria) ->
	User = hawk_server_app:get_app_env(user, #{}),
	{Key, EValue} = Criteria,

	case get_value_from_user(maps:get(Key, User), EValue) of
		true -> {ok, User};
		_ -> {ok, #{}}
	end.

get_value_from_user({badkey,_Key}, _Value) -> false;
get_value_from_user(Value, EValue) when is_list(Value) ->
	Mem = lists:member(EValue, Value),
	if
		Mem == true -> true;
		true        -> false
	end;
get_value_from_user(Value, EValue) -> Value == EValue.