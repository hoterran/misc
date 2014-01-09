-module(call_cast_test).
-behaviour(gen_server).

-export([start_link/1, test_call/2, test_cast/2, test_call/3, test_info/2, test/0, z1/1, z2/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER,	?MODULE).

%100 count
%1369 - 891 
%1000 count 
%12447 - 21920

-define(N, 1000).

-record(state, {arg, c1=0, c2=0, fd, starttime}).

%%
%%CALLER
%%

test() ->
	{ok, Pid} = start_link([abc, efg]),

    lists:foreach(
        fun(F) ->
            {T, _} = timer:tc(?MODULE, F, [Pid])
        end 
        ,   
        [
			z1,z2
        ]
    ),
	test_info(Pid, stop).

z1(Pid) ->
	lists:foreach(fun(S) -> test_call(Pid, helloyou) end, lists:seq(1, ?N+1)).

z2(Pid) ->
	lists:foreach(fun(S) -> test_cast(Pid, helloyou) end, lists:seq(1,?N+1)).

start_link(Args) ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [Args], []).

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
	{ok, Fd} = file:open("/tmp/a.log", [write, raw]),
	{ok, #state{arg=Args, fd = Fd}}.

handle_call(Msg, {From,_}, State) ->
	Reply = Msg,
	C = State#state.c1,	
	Fd = State#state.fd,
	StartTime = State#state.starttime,

	ok = file:write(Fd, lists:duplicate(10, "aaaaaaa")),

	if 
	C == 0 ->
		{reply, call_ok, State#state{starttime=erlang:now(), c1=C+1}};
	C == ?N ->
		io:format("call spend ~p ~p ~n", 
			[timer:now_diff(erlang:now(), StartTime), C]),
		{reply, call_ok, State};
	true ->
		{reply, call_ok, State#state{c1=C+1}}
	end.

handle_cast(Msg, State) ->
	C = State#state.c2,
	Fd = State#state.fd,
	StartTime = State#state.starttime,

	ok = file:write(Fd, lists:duplicate(10, "aaaaaaa")),

	if 
	C == 0 ->
		{noreply, State#state{starttime=erlang:now(), c2=C+1}};
	C == ?N ->
		io:format("cast spend ~p ~p ~n", 
			[timer:now_diff(erlang:now(), StartTime), C]),
		{noreply, State};
	true ->
		{noreply, State#state{c2=C+1}}
	end.

handle_info(Info, State) ->
	{stop, normal, State}.

terminate(Reason, _State) ->
	ok.

code_change(_OldVsn, _State, _Extra) ->
	{ok, _State}.

