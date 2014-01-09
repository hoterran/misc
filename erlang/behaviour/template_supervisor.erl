-module(template_supervisor).

-export([start_link/1, which_children/0, start_child/1]).
-export([init/1]).
 
start_link(Args) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Args]).

%%	SupFlags	: {Strategy, MaxIntensity, Period} 
%%	ChildSpec	: [{Id, StartFunc, Restart, Shutdown, Type, Modules}, ....]
%%		Id = term()
%%		StartFunc = {M, F, A}
%%        M = F = atom()
%%        A = [term()]
%%    Restart = permanent | transient | temporary
%%    Shutdown = brutal_kill | integer() >=0 | infinity
%%    Type = worker | supervisor
%%    Modules = [Module] | dynamic
%%        Module = atom()

%init([Args]) ->
%	io:format("Sup Init Args:~p~n", [Args]),
%    {ok, {{one_for_one, 2, 60}, 
%        [
%			{child1, {template_child, start_link, [Args]}, 
%			permanent, brutal_kill, worker, [template_child]},
%			{child2, {template_child, start_link, [Args]}, 
%			permanent, brutal_kill, worker, [template_child]}
%		]
%	}}.
%
%start_child(Args) ->
%	Spec = {Args, {template_child, start_link, [Args]},
%			permanent, brutal_kill, worker, [Args]},
%	supervisor:start_child(?MODULE, Spec).
%
init([Args]) ->
	io:format("Sup Init Args:~p~n", [Args]),
    {ok, {{simple_one_for_one, 2, 60}, 
        [
			{child1, {template_child, start_link, []}, 
			permanent, brutal_kill, worker, [template_child]}
		]
	}}.

start_child(Args) ->
	io:format("Start child~n"),
	supervisor:start_child(?MODULE, [Args]).

which_children() ->
	supervisor:which_children(?MODULE).

