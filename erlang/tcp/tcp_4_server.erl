-module(tcp_4_server).

-export([start/0, accept/1, recv/1]).

start() ->
	{ok, ListenSocket} = gen_tcp:listen(4321, [binary, {active, true}, {reuseaddr, true}]),
	accept(ListenSocket).

accept(ListenSocket) ->
	io:format("here~n"),
	{ok, Socket} = gen_tcp:accept(ListenSocket),
	spawn(?MODULE, accept, [ListenSocket]),
	recv(Socket).

recv(Socket) ->
	receive
		{tcp, Socket, Data} ->
			io:format("receive data: [~p]~n", [Data]),
			gen_tcp:send(Socket, "hahah"),
			recv(Socket);
		{tcp_closed, Socket} ->
			io:format("socket close~n"),
			gen_tcp:close(Socket)
	end.
