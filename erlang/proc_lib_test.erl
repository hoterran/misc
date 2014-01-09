-module(proc_lib_test).

-export([start/0, init/1]).

start() ->
	{ok, Pid} = proc_lib:start(?MODULE, init, [self()]),
	io:format("Fork a Process ~p", [Pid]).

init(Parent) ->
	timer:sleep(1000),
	io:format("Parent Must Wait Child Process ack~n"),
	proc_lib:init_ack(Parent, {ok, self()}),
	timer:sleep(100000).


