-module(tcp_1_server).

-export([start/0, wait_connect/2]).

%% false, need recv 
start() ->
	{ok, ListenSocket} = gen_tcp:listen(1234, [binary, {active, false}, {reuseaddr, true}]),
	wait_connect(ListenSocket, 0),
	ok.

wait_connect(ListenSocket, Count) ->
	{ok, Socket} = gen_tcp:accept(ListenSocket),
	spawn(?MODULE, wait_connect, [ListenSocket, Count + 1]),
	loop(Socket, <<>>, Count).

loop(Socket, Buffer, Count) ->
	case gen_tcp:recv(Socket, 0) of 
		{ok, RecvBinary} -> 
			io:format("recv ~p~n", [RecvBinary]),
			Binary = list_to_binary(binary_to_list(Buffer) ++ binary_to_list(RecvBinary)),
			handleMessage(Socket, Binary);
		{error, closed} ->
			io:format("close~n"),
			gen_tcp:close(Socket),
			ok
	end.

handleMessage(Socket, Binary) ->
	case Binary of
		<<Tag:32, Len:32, Type:8, Body:Len/binary-unit:8, Tail:32, Remain/binary>> ->
			io:format("data: ~p ~n", [Body]), 
			io:format("remain ~p~n", [Remain]),
			handleMessage(Socket, Remain);
		<<_/binary>> ->
			loop(Socket, Binary, 0)
	end.
