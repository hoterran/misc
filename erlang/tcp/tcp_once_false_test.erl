-module(tcp_once_false_test).
-behaviour(gen_server).

-export([start_link/1, test_call/2, test_cast/2, test_call/3, test_info/2]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([test/0, test_once/1, test_false/1, test_true/1]).

-define(SERVER,	?MODULE).

-record(state, {arg, lsock}).

%%
%%CALLER
%%
test() ->
	{ok, Pid} = start_link([abc, efg]),
	test_once(Pid),
	test_false(Pid),
	test_true(Pid),
	test_info(Pid, info).

test_once(Pid) ->
	{ok, Socket} = gen_tcp:connect("127.0.0.1", 1234, [binary, {packet, 0}, {active, once}]),

	%let server send data
	test_call(Pid, send),
	timer:sleep(3000),

	io:format("~p~n", [erlang:process_info(self(), message_queue_len)]),
	T1 = erlang:now(),
	loop2(Socket),
	io:format("once ~p~n", [timer:now_diff(erlang:now(), T1)]).

loop2(Socket) ->
	receive 
		{tcp, Socket, Data} ->
			%io:format("1 ~p~n", [byte_size(Data)]),
			inet:setopts(Socket, [{active, once}]),
			loop2(Socket)
	after 100 ->
		ok
	end.

test_true(Pid) ->
	{ok, Socket} = gen_tcp:connect("127.0.0.1", 1234, [binary, {packet, 0}, {active, true}]),

	%let server send data
	test_call(Pid, send),
	timer:sleep(3000),

	io:format("~p~n", [erlang:process_info(self(), message_queue_len)]),
	T1 = erlang:now(),
	loop3(Socket),
	io:format("true ~p~n", [timer:now_diff(erlang:now(), T1)]).

loop3(Socket) ->
	receive 
		{tcp, Socket, Data} ->
			%io:format("3 ~p~n", [byte_size(Data)]),
			loop3(Socket)
	after 100 ->
		ok
	end.

test_false(Pid) ->
	{ok, Socket} = gen_tcp:connect("127.0.0.1", 1234, [binary, {packet, 0}, {active, false}]),

	%let server send data
	test_call(Pid, send),
	timer:sleep(3000),

	io:format("~p~n", [erlang:process_info(self(), message_queue_len)]),
	T1 = erlang:now(),
	loop(Socket),
	io:format("false ~p~n", [timer:now_diff(erlang:now(), T1)]).

loop(Socket) ->
	case gen_tcp:recv(Socket, 1460, 100) of
		{error, timeout} ->
			ok;
		{ok, Data}  ->
			%io:format("2 ~p~n", [byte_size(Data)]),
			loop(Socket)
	end.

start_link(Args) ->
	gen_server:start_link(?MODULE, [Args], []).

test_call(Name, Call) ->
	gen_server:call(Name, Call).

test_call(Name, Call, Timeout) ->
	gen_server:call(Name, Call, Timeout).

test_cast(Name, Cast) ->
	gen_server:cast(Name, Cast).

test_info(Name, Info) ->
	Name ! {Info}.

%%
%% CALLBACK
%%

init([Args]) ->
	io:format("Args is ~p~n", [Args]),
	{ok, Ls} = gen_tcp:listen(1234, [binary, {reuseaddr, true}, {packet, 0}]),
	{ok, #state{lsock=Ls}}.

handle_call(Msg, {From,_}, State) ->
	Reply = Msg,
	Ls = State#state.lsock,
	{ok, Client} = gen_tcp:accept(Ls),
	gen_tcp:send(Client, list_to_binary(lists:duplicate(10000000, "aaaaaaaaaa"))),
	{reply, call_ok, State}.

handle_cast(Msg, State) ->
	io:format("cast ~p~n", [Msg]),
	{noreply, State}.

handle_info(Info, State) ->
	io:format("info ~p~n", [Info]),
	{stop, normal, State}.

terminate(Reason, _State) ->
	io:format("stop ~p~n", [Reason]),
	ok.

code_change(_OldVsn, _State, _Extra) ->
	{ok, _State}.

