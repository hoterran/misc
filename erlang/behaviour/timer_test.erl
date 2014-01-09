-module(timer_test).
-behaviour(gen_server).

-export([start_link/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER,	?MODULE).

-record(state, {arg}).

%%
%%CALLER
%%

start_link(Args) ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [Args], []).

%%
%% CALLBACK
%%

init([Args]) ->
	io:format("Args is ~p~n", [Args]),
%	{ok, #state{arg=Args}}.
	I = 10,
	erlang:start_timer(I * 1000, self(), {heartbeat}),
	{ok, #state{arg=I}}.
	%{ok, #state{arg=Args}, 10000}.

handle_call(Msg, {From,_}, State) ->
	Reply = Msg,
	io:format("call ~p ~p~n", [Msg, From]),
	{reply, call_ok, State}.

handle_cast(Msg, State) ->
	io:format("cast ~p~n", [Msg]),
	{noreply, State}.

handle_info(timeout, State) ->
	io:format("info timeout ~n"),
	{noreply, State, 1000};

handle_info({timeout, TRef, {heartbeat}}, State) ->
	io:format("info ~p~n", [TRef]),
	I = State#state.arg - 1,
	if I >= 0 ->
		erlang:start_timer(I * 1000, self(), {heartbeat});
	true ->
		ok
	end,
	{noreply, #state{arg = I } }.

terminate(Reason, _State) ->
	io:format("stop ~p~n", [Reason]),
	ok.

code_change(_OldVsn, _State, _Extra) ->
	{ok, _State}.

