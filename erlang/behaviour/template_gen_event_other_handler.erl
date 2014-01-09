-module(template_gen_event_other_handler).

-compile(export_all).

%callbacks
-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {zzz}).

init(Arg) ->
	io:format("init arg template_gen_event_other_handler ~p ~n", Arg),
	State = #state{zzz = Arg},
	{ok, State}.

handle_event({log, E}, State) ->
	timer:sleep(1000),
	io:format("handle_event2 ~p ~n", [State]),
	{ok, []}.

handle_call(report, State) ->
	io:format("handle_call ~n"),
	{ok, State, "fuck"}.

handle_info(report, State) ->
	io:format("handle_info ~n"),
	{ok, State};

handle_info(Info, State) ->
	io:format("handle_info2 ~p ~n", [Info]),
	{ok, State}.

terminate(release, State) ->
	io:format("recent log handler2 release~n", [State]),
	ok;

terminate(Args, State) ->
	io:format(" delete handler2 here ~p ~p ~n", [Args, State]),
	ok.

code_change(_Vsn, State, Extra) ->
	ok.
