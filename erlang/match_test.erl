-module(match_test).

-export([start/0]).

start() ->
	init_dynamic("aaaa", [{aaaaa}, {bbbbb}]),
	init_dynamic("aaaa", {aaaaa}).

init_dynamic(State, [_, _]) ->
	io:format("aaa");

init_dynamic(_State, StartSpec) ->
	io:format("bbb").
