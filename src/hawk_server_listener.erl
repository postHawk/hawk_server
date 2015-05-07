%% @author maximilian
%% @doc @todo Add description to hawk_server_listener.


-module(hawk_server_listener).
-behaviour(gen_server).
-behaviour(ranch_protocol).

-include("mac.hrl").

-export([start_link/4]).
-export([init/4]).
-export([init/1]).
-export([handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {socket, transport, headers}).

start_link(Ref, Socket, Transport, Opts) ->
	proc_lib:start_link(?MODULE, init, [Ref, Socket, Transport, Opts]).

init(Ref, Socket, Transport, _Opts = []) ->
	ok = proc_lib:init_ack({ok, self()}),
	ok = ranch:accept_ack(Ref),
	ok = Transport:setopts(Socket, [{active, once}]),
	gen_server:enter_loop(?MODULE, [],
		#state{socket=Socket, transport=Transport}).

%% ====================================================================
%% API functions
%% ====================================================================

init([]) -> {ok, undefined}.

handle_info({ssl, Socket, Data}, State=#state{socket=Socket, transport=Transport}) ->
	Transport:setopts(Socket, [{active, once}]),
 	
	{ok, {http_request,Method,{abs_path, _URL},_}, H} = erlang:decode_packet(http, Data, []),
	[Headers, {body, _Body}] = hawk_server_lib:parse_header(H),
	case set_client({Method, Headers}) of
		{ok, false} -> 
			Transport:send(Socket, ?ERROR_DOMAIN_NOT_REGISTER);
		{ok, Client, Host} ->
			Transport:setopts(Socket, [{active, once}]),
			Transport:controlling_process(Socket, Client),
			gen_fsm:send_event(Client, {socket_ready, Socket, binary_to_list(Host), Transport}),
 			gen_fsm:send_event(Client, {data, Data})
	end,

	{stop, normal, State};

handle_info({ssl_closed, _Socket}, State) ->
	{stop, normal, State};
handle_info({ssl_error, _, Reason}, State) ->
	{stop, Reason, State};
handle_info(timeout, State) ->
	{stop, normal, State};
handle_info(_Info, State) ->
	{stop, normal, State}.

handle_call(_Request, _From, State) ->
	{reply, ok, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.


%% ====================================================================
%% Internal functions
%% ====================================================================
get_supervisor_by_name(Name) ->
	
	case gproc:where({n, l, Name}) of
		undefined -> 
			supervisor:start_child(hawk_server_clients, [Name]);
		Pid ->
			{ok, Pid}
	end.

set_client({'GET', Headers}) ->
	Host = binary:replace(proplists:get_value(<<"Origin">>, Headers) ,[<<"https://">>, <<"http://">>],<<"">>),
	User = ?get_user_by_domain(Host),

	%select or run supervisor for client
	case User of
		{ok, false} ->
			{ok, false};
		{ok, U} ->
			{Login} = bson:lookup (login, U),
			{U_hosts} = bson:lookup(domain, U),
			{ApiKey} = bson:lookup(key, U),
			
			{ok, Sup} = get_supervisor_by_name(Login),
			{ok, Client} = supervisor:start_child(Sup, [Login]),
			
			dets:insert(main_user_data, {Login, U_hosts, ApiKey}),
			
			{ok, Client, Host}
	end;

set_client({'POST', Headers}) ->
	{ok, Sup} = get_supervisor_by_name(<<"post_sup">>),
	{ok, Client} = supervisor:start_child(Sup, [<<"post_sup">>]),
	Host = binary:replace(proplists:get_value(<<"Origin">>, Headers) ,[<<"https://">>, <<"http://">>],<<"">>),
	{ok, Client, Host}.



