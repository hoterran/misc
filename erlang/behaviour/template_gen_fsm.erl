-module(template_gen_fsm).
-behaviour(gen_fsm).

-export([start_link/1, 
		state1/2,
		state2/3,
		state2/2,
		test_send_event/2, 
		test_sync_send_event/2, 
		test_send_all_event/2,	
		test_sync_send_all_event/2,	
		test_info/2,
		test/0,
		test_misc/0
		]).

-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-define(SERVER,	?MODULE).

-record(state, {arg}).

%%
%%CALLER
%%

test_misc() ->
	gen_fsm:start_timer(10000, "fuck~n").

test() ->
	{ok, Pid} = start_link([abc, def]),
	test_send_event(Pid, gotostate2),

	test_sync_send_event(Pid, gotostate1),

	test_send_all_event(Pid, gotostate1),
	test_send_all_event(Pid, gotostate2),

	test_sync_send_all_event(Pid, gotostate1),
	test_sync_send_all_event(Pid, gotostate2),

	test_info(Pid, some_msg).

start_link(Args) ->
	gen_fsm:start_link({local, ?SERVER}, ?MODULE, [Args], []).

test_send_event(Name, Event) ->
	gen_fsm:send_event(Name, Event).

test_sync_send_event(Name, Event) ->
	gen_fsm:sync_send_event(Name, Event).

test_send_all_event(Name, Event) ->
	gen_fsm:send_all_state_event(Name, Event).

test_sync_send_all_event(Name, Event) ->
	gen_fsm:sync_send_all_state_event(Name, Event).

test_info(Name, Info) ->
	Name ! {Info}.

%%
%% CALLBACK
%%

init([Args]) ->
	io:format("Fsm Args is ~p, current state is ~p ~n", [Args, state1]),
	{ok, state1, #state{arg=Args}, 10000}.

state1(timeout, State) ->
	io:format("fuck timeout1~n"),
	{next_state, state2, State, -1};

state1(gotostate2, State) ->
	io:format("receive event gotostate2, From state1 -> state2~n"),
	{next_state, state2, State}.

state2(timeout, State) ->
	io:format("fuck timeout2~n"),
	{next_state, state2, State}.

state2(gotostate1, From, State) ->
	io:format("receive sync event gotostate1[~p], From state2 -> state1~n", [From]),
	%%{next_state, state1, State}.
	{reply, sync_event_ok, state1, State}.

handle_event(Msg, StateName, StateData) ->
	if Msg =:= gotostate1 ->
		NewState = state1;
	true ->
		NewState = state2
	end,
	{next_state, NewState, StateData}.

handle_sync_event(Msg, From, StateName, StateData) ->
	if Msg =:= gotostate1 ->
		NewState = state1;
	true ->
		NewState = state2
	end,
	io:format("receive sync event ~p[~p], From state ~p -> ~p~n", [Msg, From, StateName, NewState]),
	{reply, sync_all_event_ok,  NewState, StateData}.

handle_info(Info, StateName, StateData) ->
	io:format("info ~p~n", [Info]),
	{stop, normal, StateData}.

terminate(Reason, _StateName, _StateData) ->
	io:format("stop ~p~n", [Reason]),
	ok.

code_change(_OldVsn, _State, _StateData, _Extra) ->
	{ok, _State}.

