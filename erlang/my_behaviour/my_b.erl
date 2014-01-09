-module(my_b).
-export([start/1, stop/0]).
-export([behaviour_info/1]).

%%do null just
%%just warning?
behaviour_info(callbacks) -> [{init, 1}, {handle_cast, 2}];
behaviour_info(_Other) -> undefined.

start(Mod) ->
	%% start from 0
	State = Mod:init(0),
	{ok, State2} = Mod:handle_cast(add, State),
	io:format("state ~p~n", [State2]),
	State.

stop() ->
	stop.

