-module(use_b).
-behaviour(my_b).
-export([init/1, handle_cast/2]).

init(State) -> 
	io:format("init ~p ~n", [State]),
	State.

handle_cast(Request, State) -> 
	io:format("request to callback ~p~n", [Request]),
	State2 = State + 1,
	{ok, State2}.

