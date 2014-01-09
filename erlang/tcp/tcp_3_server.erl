-module(tcp_3_server).

-export([start_nono_server/0]).

start_nono_server() ->
	{ok, Listen} = gen_tcp:listen(12345, [binary, {packet, 4}, {reuseaddr, true}, {active, true}]),
	{ok, Socket} = gen_tcp:accept(Listen),
	loop(Socket).

loop(Socket) ->
	receive
		{tcp, Socket, Bin} ->
			io:format("Server received binary = ~p~n", [Bin]),
			Str = binary_to_term(Bin),
			[A1, B1, C1] = Str,
			io:format("Server (unpacked) ~p ~p ~p ~n", [A1, B1, C1]),
			%%Reply = lib_misc:string2value(Str),
			%%io:format("Server replying = ~p~n", [Reply]),
			gen_tcp:send(Socket, term_to_binary("hahah")),
			loop(Socket);
		{tcp_closed, Socket} ->
			io:format("Server socket closed~n")
	end.
