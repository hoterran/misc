-module(mysql_proxy_qps).
-behaviour(gen_fsm).

%%-export([start_link/2, getqps/2, calcqps/2]).
-compile(export_all).

-export([init/1, handle_event/3, handle_info/3, handle_sync_event/4, code_change/4, terminate/3]).

-define (SERVER, ?MODULE).

-define (STEPSECONDS, 10000). %10 seconds
-define (STEPS,	6).				%6 tims 

-record(state, {parentpid, getqpstime, user, step, qps}).
-record(user_proxy_qps, {key_ref, username, nodename, key, qps, qpstime}).
-record(user_proxy_conn, {key_ref, username, nodename, count}).

%% start here
start_link(ParentPid, User) ->
	gen_fsm:start(?MODULE, [ParentPid, User], []).

init([ParentPid, User]) ->
	io:format("init ~p ~p ~n", [ParentPid, self()]),
	State = #state{parentpid = ParentPid, step=0, user=User, qps=0},
	process_flag(trap_exit, true),
	link(ParentPid),
	{ok, getqps, State, 1000}.

getqps(timeout, State=#state{parentpid=ParentPid, step=Step}) ->

	Time = calendar:datetime_to_gregorian_seconds(erlang:localtime()),
	%io:format("get qps ~n"),
	%%io:format("timeout lock ~n"),
	ParentPid ! {getqps , self()},
	{next_state, calcqps, 
		State#state{step=Step+1, getqpstime=Time}}.

calcqps(start_calc, State=#state{parentpid=ParentPid, qps=Qps, step=Step, getqpstime=Time, user=User}) ->

	%%update qps
	Key = Step rem ?STEPS,
	update_qps(User, node(), Key, Qps),

	%%calc qps
	NodesQps = sum_qps(User),
	%% true qps
	AverageQps = NodesQps / ?STEPS * 1000 / ?STEPSECONDS,
	ParentPid ! {putqps, AverageQps, mysql_proxy_qps:get_conn(User)},
	io:format("~p put qps [~p ~p] ~n", [self(), AverageQps, NodesQps]),
	Time2 = calendar:datetime_to_gregorian_seconds(erlang:localtime()),
	SleepTime = ?STEPSECONDS - (Time2 - Time),
	{next_state, getqps, State, SleepTime}.

code_change(_OldVsn, State, StateData, _Extra) ->
	{ok, State, StateData}.

handle_event(Event, StateName, StateData) ->
	{stop, StateName, StateData}.

handle_info({qps, Qps}, StateName, State) ->
	io:format("receive qps [~p, ~p]~n", [Qps, StateName]),
	gen_fsm:send_event(self(), start_calc),
	{next_state, StateName, State#state{qps=Qps}};

handle_info({'EXIT', Pid, Reason}, StateName, State) ->
	io:format("proxy exit~n"),
	{stop, StateName, State};

handle_info(_Info, StateName, StateData) ->
	{stop, StateName, StateData}.

handle_sync_event(Event, From, StateName, StateData) ->
	{stop, StateName, StateData}.

terminate(_Reason, StateName, StateData) ->
	ok.

%%%%mensia%%%%

update_qps(User, Node, Key, Qps) ->
    F = fun() -> 
		%token is for next loop
		{{Y,M,D}, {H,MI,S}} = erlang:localtime(),
		Token = integer_to_list(Y) ++ integer_to_list(M) ++ integer_to_list(D)
			++ integer_to_list(H) ++ integer_to_list(MI) ++ integer_to_list(round(S div 10)),

        Recs = mnesia:index_read(user_proxy_qps, User, username),
        Rec = [X|| X <-Recs, X#user_proxy_qps.nodename =:=Node, X#user_proxy_qps.key=:=Key],
        case Rec of    
            [] -> mnesia:write(#user_proxy_qps{key_ref = make_ref(),
                    username = User, nodename=Node, key=Key, qps=Qps, qpstime=Token});
            [One] -> 
				io:format("here~p ~p ~n", [One, Token]),
				case One#user_proxy_qps.qpstime =:= Token of
					false ->
						%%new token
						io:format("new token~n"),
						mnesia:write(One#user_proxy_qps{qps=Qps, qpstime=Token});
					true ->
						io:format("old token~n"),
						mnesia:write(One#user_proxy_qps{qps=One#user_proxy_qps.qps + Qps})
				end
        end 
    end,
    case mnesia:transaction(F) of
        {atomic, _} -> {ok, []};
        {aborted, Reason} -> io:format("update pqs error: ~p", [Reason]), {error, Reason}
    end.
 
sum_qps(User) ->                
    F = fun() ->                
        Recs = mnesia:index_read(user_proxy_qps, User, username),
        lists:foldl(fun(X, Sum) -> X#user_proxy_qps.qps + Sum end, 0, Recs)
    end,                        
    case mnesia:transaction(F) of
        {atomic, Qps} -> Qps
    end.

%%all nodes
get_conn(User) ->
	F = fun() ->
		Recs = mnesia:index_read(user_proxy_conn, User, username),
        lists:foldl(fun(X, Sum) -> X#user_proxy_conn.count + Sum end, 0, Recs),
		case Recs of 
			[] ->
				0;
			[One] ->
				One#user_proxy_conn.count
		end
	end,
    case mnesia:transaction(F) of
        {atomic, C} -> C
    end.

%% current node
add_conn(User) ->
	F = fun() ->
		Recs = mnesia:index_read(user_proxy_conn, User, username),
        Rec = [X|| X <-Recs, X#user_proxy_conn.nodename =:= node()],
		case Rec of 
			[] ->
				mnesia:write(#user_proxy_conn{username=User, nodename=node(), count=1});
			[One] ->
				mnesia:write(One#user_proxy_conn{count=One#user_proxy_conn.count+1})
		end
	end,
    case mnesia:transaction(F) of
        {atomic, _} -> {ok, []};
		{_, Reason} -> {error, Reason}
    end.

remove_conn(User) ->
	F = fun() ->
		Recs = mnesia:index_read(user_proxy_conn, User, username),
        Rec = [X|| X <-Recs, X#user_proxy_conn.nodename =:= node()],
		case Rec of 
			[] ->
				io:format("fuck");
			[One] ->
				mnesia:write(One#user_proxy_conn{count=One#user_proxy_conn.count-1})
		end
	end,
    case mnesia:transaction(F) of
        {atomic, _} -> {ok, []};
		{_, Reason} -> {error, Reason}
    end.
