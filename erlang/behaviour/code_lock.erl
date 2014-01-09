-module(code_lock).
-behaviour(gen_fsm).

-export([start_link/1]).
-export([button/1]).
-export([locked/2, open/2]).

-export([init/1, handle_event/3, handle_info/3, handle_sync_event/4, code_change/4, terminate/3]).

-define (SERVER, ?MODULE).

%% start here
start_link(Code) ->
	gen_fsm:start_link({local, ?SERVER}, ?MODULE, Code, []).

%%Code is passwd
%%[] incomplete passwd insert last time
init(Code) ->
	io:format("init~n"),
	{ok, locked, {[], Code}}.

%% Event = button
%% StateData = Digit
button(Digit) ->
	gen_fsm:send_event(?SERVER, {button, Digit}).

%% Sofar is last incomplete code
%%
locked({button, Digit}, {Sofar, Code}) ->
	io:format("call locked ~p ~p ~n", [Sofar ++ Digit, Code]),

	case Sofar ++ Digit of
		Code ->
			do_unlock(),
			{next_state, open, {[], Code}, 3000};
		Incomplete when length(Incomplete) < length(Code) ->
			io:format("Incomplete code ~n"),
			{next_state, locked, {Incomplete, Code}};
		_ ->
			io:format("wrong code ~n"),
			{next_state, locked, {[], Code}}
	end.
			
open(timeout, State) ->
	io:format("timeout lock ~n"),
	do_lock(),
	{next_state, locked, State}.

do_lock() ->
	io:format("lock~n").

do_unlock() ->
	io:format("unlock~n").

code_change(_OldVsn, State, StateData, _Extra) ->
	{ok, State, StateData}.

handle_event(Event, StateName, StateData) ->
	{stop, StateName, StateData}.

handle_info(_Info, StateName, StateData) ->
	io:format("ooooooooo~n"),
	{stop, StateName, StateData}.

handle_sync_event(Event, From, StateName, StateData) ->
	{stop, StateName, StateData}.

terminate(_Reason, StateName, StateData) ->
	ok.

