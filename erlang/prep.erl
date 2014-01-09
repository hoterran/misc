-module(prep).
-export([start/0]).

-define(LOG(X), io:format("~p ~p ~p~n", [?MODULE, ?LINE, X])).

-ifdef(test).
start() ->
	?LOG(abc).
-else.
start() ->
	compile_use_test.
-endif.
