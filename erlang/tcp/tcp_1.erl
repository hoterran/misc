-module(tcp_1).

-export([client/2, send/2]).

client(Host, Data) ->
	{ok, Socket} = gen_tcp:connect(Host, 1234, [binary, {packet, 0}]),
	send(Socket, Data).

send(Socket, Data) ->
	%% header 4byte:aaaa
	%% datalength 4byte: int
	%% type 1byte: char
	%% data
	%% tail 4byte:bbbb

	Len = length(Data),
	Binary = list_to_binary("aaaa" ++ binary_to_list(<<Len:32>>) ++ "1" ++ Data ++ "bbbb"),
	gen_tcp:send(Socket, Binary),
	gen_tcp:send(Socket, Binary).
		
