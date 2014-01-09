-module(mysql_proxy_qps_user_sup).
-behaviour(supervisor).

-export([start_link/0, start_child/1]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
	supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%dont use {local, ?SERVER}
start_child(User) ->
	supervisor:start_child(?SERVER, [User]).

init([]) ->
	Server = {mysql_proxy_qps_user,  
		{mysql_proxy_qps_user, start_link, []},
		temporary, brutal_kill, 
		worker, [mysql_proxy_qps_user]},
	Children = [Server],
	RestartStrategy = {simple_one_for_one, 0, 1},
	{ok, {RestartStrategy, Children}}.

