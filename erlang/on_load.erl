-module(on_load).

-on_load(test/0).

test() ->
	io:format("xxx~n").
