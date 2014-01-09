-module(template_gen_event).
-behaviour(gen_event).

-compile(export_all).

%api
-export([start/0, stop/0, notify/1, sync_notify/1, call/0, delete_handler/1, info/1]).

%callbacks
-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2, code_change/3]).
-define(NAME, logger_manager).

-record(state, {arg}).

start() ->
	case gen_event:start_link({local, ?NAME}) of
		Ret = {ok, _Pid} ->
			io:format("do none~p ~n", [_Pid]),
			gen_event:add_handler(?NAME, ?MODULE, [aaa]),
			gen_event:add_handler(?NAME, ?MODULE, [ccc]),
			gen_event:add_handler(?NAME, template_gen_event_other_handler, [bbb]),
			Ret;
		Other ->
			Other
	end.

stop() ->
	gen_event:stop(?NAME).

notify(E) ->
	gen_event:notify(?NAME, {log, E}),
	io:format("notify~n").

sync_notify(E) ->
	gen_event:sync_notify(?NAME, {log, E}),
	io:format("sync notify~n").

info(Msg) ->
	logger_manager ! Msg.

call() ->
	gen_event:call(?NAME, ?MODULE, report).

delete_handler(Module) ->
	gen_event:delete_handler(?NAME, Module, release).

init(Arg) ->
	io:format("init arg ~p ~n", [Arg]),
	State = #state{arg = Arg},
	{ok, State}.

handle_event({log, E}, State) ->
	timer:sleep(2000),
	io:format("handle_event ~p ~n", [State]),
	{ok, []}.

handle_call(report, State) ->
	io:format("handle_call ~p ~n", [State]),
	{ok, "fuck", State}.

handle_info(report, State) ->
	io:format("handle_info ~n"),
	{ok, State};

handle_info(Info, State) ->
	io:format("handle_info ~p ~p ~n", [Info, State]),
	{ok, State}.

terminate(release, State) ->
	io:format("recent log handler release~p ~n", [State]),
	ok;

terminate(Args, State) ->
	io:format("delete handler1 here ~p ~p ~n", [Args, State]),
	ok.

code_change(_Vsn, State, Extra) ->
	ok.
