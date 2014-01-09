-module(tcp_3).

-export([nano_client_eval/1]).

nano_client_eval(Str) ->
	{ok, Socket} = gen_tcp:connect("localhost", 12345, [binary, {packet, 4}]),
	%% [xyz, abcdef, Str]
	List = [ xyz | [ abcdef | [Str]  ] ],
	ok = gen_tcp:send(Socket, term_to_binary(List)),
	loop(Socket).

loop(Socket) ->
	io:format("start loop~n"),
	receive
		{tcp, Socket, Bin} ->
			io:format("Client received binary = ~p~n", [Bin]),
			Val = binary_to_term(Bin),
			io:format("Client resut = ~p~n", [Val]),
			loop(Socket);
		{tcp_closed, Socket} ->
			io:format("socket close~n"),
			gen_tcp:close(Socket)
	end.

