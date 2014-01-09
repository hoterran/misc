-module(spawn_link_test).

-compile(export_all).

start() ->
	Pid = spawn(?MODULE, loop, []),
	link(Pid),
	receive
	  {'EXIT', Pid, Reason} -> io:format("~p", [Reason])
	after 10000 ->
		io:format("ok~n")
	end.

loop() ->
	io:format("bbb~n"),
	timer:sleep(5000).
