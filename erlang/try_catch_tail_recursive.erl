-module(try_catch_tail_recursive).
-compile(export_all).

% no tail
t3() ->
	t3("list").

t3(T) ->
	timer:sleep(100),
	"list" ++ t3(T).

% tail 
t2() ->
	timer:sleep(100),
	t2().

% no tail
t1() ->
	try
		timer:sleep(100),
		t1()
	catch
		_:_ ->
			ok
	end.

start3() ->
	Pid = spawn(?MODULE, t3, []),
	loop(Pid).

start1() ->
	Pid = spawn(?MODULE, t1, []),
	loop(Pid).

loop(Pid) ->
	io:format("~p~n", [erlang:process_info(Pid, [memory])]),
	%io:format("backtrace:~w~n~n", [erlang:process_display(self(), backtrace)]),
	timer:sleep(100),
	loop(Pid).

start2() ->
	Pid = spawn(?MODULE, t2, []),
	loop(Pid).

