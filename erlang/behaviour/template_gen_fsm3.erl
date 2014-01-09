-module().
-behaviour(gen_fsm).

%%API
-export([start_link/0]).

%%gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	terminate/2, code_change/3]).

start_link() ->
	gen_fsm:start_link({local, ?MODULE}, ?MODULE, [], []).

-record(state, {}).

%%
%% {ok, StateName, StateData, Timeout}	|
%% {ok, StateName, StateData, hibernate} |
%% {stop, Reason}
%%
init() ->
	{ok, state_name, {}}.

%%
%% {next_state, NextStateName, NewStateData} |
%% {next_state, NextStateName, NewStateData, Timeout} |
%% {next_state, NextStateName, hibernate}
%% {stop, Reason, NewStateData}
%%
state_name(Event, StateData) ->
	{next_state, next_state_name, {}}.

next_state_name() ->
	{next_state, state_name, {}}.

%%
%% {reply, Reply, State} 
%%
%% Msg  = {}
handle_call(Msg, _From, State) ->
	Reply = ok, 
	{reply, ok, State}.

%%
%%  {noreply, State} | {noreply, State, Timeout} | {stop, Reason, State}
%%
handle_cast(_Msg, State) ->
	{noreply, State}.

%%
%% {noreply, State} | {noreply, State, Timeout} | {stop, Reason, State}
%%
%%_Info = timeout | {tcp, Socket, Packet} | {tcp_closed, _Socket}
handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.
