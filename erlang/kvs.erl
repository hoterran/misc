-module(kvs).
-compile(export_all).

start() -> register(kvs, spawn(fun() -> loop() end)).

store(Key, Value) -> rpc({store, Key, Value}).

lookup(Key) -> rpc({lookup, Key}).

rpc(Q) ->
	kvs ! {self(), Q},
	receive
		{kvs, Reply} ->
			Reply
	end.

loop() ->
	receive
		{From, {store, Key, Value}} ->
			put(Key, {ok, Value}),
			From ! {kvs, true},
			loop();
		{From, {lookup, Key}} ->
			From ! {kvs, get(Key)},
			loop();
		{From, {close}} ->
			io:format("close")
	end.
