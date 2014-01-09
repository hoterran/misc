-module(tcp_2).

-export([nano_get_url/1]).

nano_get_url(Host) ->
	{ok, Socket} = gen_tcp:connect(Host, 80, [binary, {packet, 0}]),
	ok = gen_tcp:send(Socket, "GET / HTTP/1.0\r\n\r\n"),
	receive_data(Socket, []).

receive_data(Socket, SoFar) ->
	receive
		{tcp, Socket, Bin} ->
			io:format("~p~n", [binary_to_list(Bin)]),
			receive_data(Socket, [Bin|SoFar]);
		{tcp_closed, Socket} ->
			list_to_binary(SoFar)
	end.
