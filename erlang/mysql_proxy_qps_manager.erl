-module(mysql_proxy_qps_manager).
-behaviour(gen_server).

-export([start_link/0, register_pid/2, unregister_pid/2]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	terminate/2, code_change/3]).

%% PROCESS
%% 1. Proxy register Manager ! {register, {self(), User}}
%% 2. Manager conclude need start this UserQps process,   start_child
%% 3. Manager send proxy's pid to UserQps
%% 4. UserQps save this pid
%% 5. UserQps loop pids call qps(timeout 0.1s), sum qps into mnesia
%% 6. Proxy unregister Manger ! {unregister}
%% 7. Manager send UserQps remove this pid

%% BEHAVIOUR
%% 1. Manager:		gen_server
%% 1-1. handle_call({register.....)
%% 1-2. register
%% 1-3. handle_call({unregister...)
%% 1-4. unregister

%% 2. UserQpsSup:	supervisor
%% 3. UserQpsChild:	gen_fsm
%% 3-1. handle_info({add })
%% 3-2. add
%% 3-3. handle_info({remove})
%% 3-4. remove
%% 3-5. loop pid

%%==========================

-define(SERVER,	?MODULE).

-record(state, {user}).

start_link() ->
	mysql_proxy_qps_user_sup:start_link(),
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

register_pid(Pid, User) ->
	gen_server:call(?MODULE, {register_pid, {Pid, User}}).

unregister_pid(Pid, User) ->
	gen_server:cast(?MODULE, {unregister_pid, {Pid, User}}).

%%
%% CALLBACK
%%
init([]) ->
	{ok, #state{user=ets:new(?MODULE, [set])}}.

handle_call({register_pid, {ProxyPid, User}}, 
	_From, State=#state{user=EtsUser}) ->
	case ets:lookup(EtsUser, User) of
		[] ->
			{ok, UserQpsPid} = mysql_proxy_qps_user_sup:start_child(User),
			ets:insert(EtsUser, {User, UserQpsPid});
		[{User, UserQpsPid}] ->
			ok
	end,
	mysql_proxy_qps_user:add_user(UserQpsPid, ProxyPid),
	{reply, UserQpsPid, State};

handle_call(_Msg, _From, State) ->
	{reply, ok, State}.

handle_cast({unregister_pid, {ProxyPid, User}}, 
	State=#state{user=EtsUser}) ->
	case ets:lookup(EtsUser, User) of 
		[] ->
			io:format("why cant find~n"),
			error;
		[{User, UserQpsPid}] ->
			mysql_proxy_qps_user:remove_user(UserQpsPid, ProxyPid),
			ets:delete(EtsUser, User)
	end,
	{noreply, State};

handle_cast(_Msg, State) ->
	{noreply, ok, State}.

handle_info(_, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

