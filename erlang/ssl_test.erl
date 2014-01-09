-module(ssl_test).
-export([start/0, client/1, accept/1]).

start() ->
   ssl:start(),
   server(4002).

server(Port) ->
    {ok, LSocket} = ssl:listen(Port, 
		[{certfile,"cert.pem"}, {keyfile, "key.pem"}, {reuseaddr, true}, {active, false}]),
    spawn(fun() -> accept(LSocket) end).
    
accept(LSocket) ->
   {ok, Socket} = ssl:transport_accept(LSocket),
   Pid = spawn(fun() ->
        io:format("Connection accepted ~p~n", [Socket]),
        loop(Socket)
   end),
   ssl:controlling_process(Socket, Pid),
   accept(LSocket).

loop(Socket) ->
   ssl:setopts(Socket, [{active, once}]),
   receive
   {ssl,Sock, Data} ->
        io:format("Got packet: ~p~n", [Data]),
        ssl:send(Sock, Data),
        loop(Socket);
   {ssl_closed, Sock} ->
        io:format("Closing socket: ~p~n", [Sock]);
   Error ->
        io:format("Error on socket: ~p~n", [Error])
   end.

client(N) ->
    {ok, Socket} = ssl:connect("localhost", 4002,  []),
    io:format("Client opened socket: ~p~n",[Socket]),
    ok = ssl:send(Socket, N),
    Value = receive
            {ssl,{sslsocket,new_ssl,_}, Data} ->
                io:format("Client received: ~p~n",[Data])
            after 2000 ->
                0
            end,
    ssl:close(Socket),
    Value.

