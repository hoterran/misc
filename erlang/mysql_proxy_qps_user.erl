-module(mysql_proxy_qps_user).

-behaviour(gen_server).

-export([start_link/1, add_conn/1, remove_conn/1, add_user/2, remove_user/2]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	terminate/2, code_change/3]).

-define(SLEEPTIME, 10000).
-define(STEPS, 6).

start_link(User) ->
	gen_server:start_link(?MODULE, [User], []).

-record(user_proxy_qps, {key_ref, username, nodename, key, qps, qpstime}).
-record(user_proxy_conn, {key_ref, username, nodename, count}).
-record(state, {etspid, steps, looptime, user}).

%why cast, this time qps possible is doing
add_user(QpsPid, ProxyPid) ->
	gen_server:cast(QpsPid, {add, ProxyPid}).	

remove_user(QpsPid, ProxyPid) ->
	gen_server:cast(QpsPid, {remove, ProxyPid}).	

%%
%% CALLBACK
%%
init([User]) ->
	{ok, #state{etspid=ets:new(list_to_atom(User), [set]), 
		steps=0, user=User, looptime=erlang:localtime()}, ?SLEEPTIME}.

handle_putqps(EtsPid, Key, SumQps, Conns) ->
	case Key of 
		'$end_of_table' ->
			ok;
		Pid ->
			io:format("put qps ~p ~p~n", [SumQps, Conns]),
			Pid ! {putqps, SumQps, Conns},
			handle_putqps(EtsPid, ets:next(EtsPid, Key), SumQps, Conns)
	end.

handle_getqps(EtsPid, Key, SumQps) ->
	case Key of 
		'$end_of_table' ->
			SumQps;	
		Pid ->
			Pid ! {getqps , self()},
			receive
				{qps, Qps} -> ok
			%%after 100 ->
		    %%	Qps = 0
			end,
			io:format("get qps ~p ~p ~n", [Pid, Qps]),
			handle_getqps(EtsPid, ets:next(EtsPid, Key),
				 SumQps + Qps)
	end.

handle_call(_Msg, _From, State) ->
	{reply, ok, State, ?SLEEPTIME}.

handle_cast({add, Pid}, State=#state{etspid=EtsPid}) ->
	io:format("add pid~n"),
	ets:insert(EtsPid, {Pid, erlang:localtime()}),
	{noreply, State, ?SLEEPTIME};

handle_cast({remove, Pid}, State=#state{etspid=EtsPid}) ->
	io:format("remove pid~n"),
	ets:delete(EtsPid, Pid),
	{noreply, State, ?SLEEPTIME};

handle_cast(_Msg, State) ->
	{noreply, State, ?SLEEPTIME}.

handle_info(timeout, State=#state{etspid=EtsPid, steps=Steps, user=User}) ->
	%% fetch each proxy qps 
	io:format("timeout ~n"),
    Time1 = calendar:datetime_to_gregorian_seconds(erlang:localtime()),

	SumQps = handle_getqps(EtsPid, ets:first(EtsPid), 0),

    %% sum qps
    Key = Steps rem ?STEPS,
    update_qps(User, node(), Key, SumQps),
    ClusterQps = sum_qps(User),

    %% true qps
    AverageQps = ClusterQps / ?STEPS * 1000 / ?SLEEPTIME,

	%% send each proxy qps
	handle_putqps(EtsPid, ets:first(EtsPid), AverageQps, get_conn(User)),

    Time2 = calendar:datetime_to_gregorian_seconds(erlang:localtime()),
    SleepTime = ?SLEEPTIME - (Time2 - Time1),
    {noreply, State#state{steps=Steps+1, looptime=Time2}, SleepTime};

handle_info(_, State) ->
	{noreply, State, ?SLEEPTIME}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%==========================mnesia================

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
				%%io:format("here~p ~p ~n", [One, Token]),
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

