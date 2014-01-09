-module(template_child).

-export([start_link/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-behaviour(gen_server).

-record(state, {}).

start_link(Args) ->
	gen_server:start_link(?MODULE, [Args], []).

init([Args]) ->
	io:format("Child Init Args:~p~n", [Args]),
	{ok, #state{}}.

handle_call(_, _, State) ->
	{reply, call_ok, State}.

handle_cast(_, State) ->
	{noreply, State}.

handle_info(_, State) ->
	{noreply, State}.

terminate(_, _State) ->
	ok. 

code_change(_OldVsn, _State, _Extra) ->
	{ok, _State}.

