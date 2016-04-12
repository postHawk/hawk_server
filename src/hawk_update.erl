%% @author maximilian
%% @doc @todo Add description to update.


-module(hawk_update).
-include("env.hrl").
-include("mac.hrl").
%% ====================================================================
%% API functions
%% ====================================================================
-export([all/0, one/1]).

all()->
	Fun = fun(Module) ->
			Res = string:str(atom_to_list(Module), "hawk_"),
			if
				Res == 0 -> true;
				true ->
					code:purge(Module), 
					code:load_file(Module),
					?DBG({"updated", Module})
			end
		end,
	lists:foreach(Fun, erlang:loaded()).

one(Module) ->
	code:purge(Module), 
	code:load_file(Module).

%% ====================================================================
%% Internal functions
%% ====================================================================


