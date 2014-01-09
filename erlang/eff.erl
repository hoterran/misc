-module(eff).
-compile(export_all).

%% test erlang effective 

start() ->
	S1 = [ {X, 1} || X <- lists:seq(1, 1000)],
	%test(lists:flatten(S1)),
	test(S1).

test(S) ->
	lists:map(
		fun(F) ->
			{T, _} = timer:tc(?MODULE, F, [S]),
			io:format("~p:~p~n", [F, T])
		end
		,
		[
			z1,
			z2,
			z3,
			z4,
			z5
		]
	).

%%bad

s1(S) ->
	naive_reverse(S).

naive_reverse([H|T]) ->
	naive_reverse(T) ++ [H];
naive_reverse([]) ->
	[].

s2(S) ->
	naive_but_ok_reverse(S, []).

naive_but_ok_reverse([H|T], Acc) ->
	naive_but_ok_reverse(T,  [H] ++ Acc);
naive_but_ok_reverse([], Acc) ->
	Acc.

s3(S) ->
	binary_copy(list_to_binary(S), <<>>).

binary_copy(<<>>, T) ->
	T;
binary_copy(<<B:8/binary,Rest/binary>>, T) ->
	binary_copy(Rest, <<T/binary, B/binary>>).

s4(S) ->
	binary_copy2(list_to_binary(S), <<>>).

binary_copy2(<<>>, T) ->
	T;
binary_copy2(<<B:8/binary,Rest/binary>>, T) ->
	binary_copy2(Rest, [T, B]).

s5(S) ->
	lists:map(
		fun(T) ->
			error_logger:info_msg(T)
		end, S).

% now and localtime
s6(S) ->
	lists:foreach(fun(_) -> erlang:now() end, S).

s7(S) ->
	lists:foreach(fun(_) -> erlang:localtime() end, S).

s8(S) ->
	lists:foreach(fun(_) -> calendar:datetime_to_gregorian_seconds(erlang:localtime()) end, S).

%% list copy ======================= from head bigger difference

a1(S) when is_list(S) ->
	copy_head(S, []).   
%	io:format("~p", [copy_head(S, [])]).   

copy_head([], Acc) ->
	Acc;
copy_head([H|List], Acc) ->
	copy_head(List, Acc ++ [H]).

a2(S) when is_list(S) ->
	lists:reverse(copy_tail(S, [])).
	%io:format("~p", [lists:reverse(copy_tail(S, []))]).

copy_tail([], Acc) ->
	Acc;
copy_tail([H|List], Acc) ->
	copy_tail(List, [H] ++ Acc).

%%=================string search ===============

%% find a string 1000 times
%% re better than multi substring

b1(S) ->
	lists:foreach(fun(F) -> search1(F, "test") end, S).

b2(S) ->
	lists:foreach(fun(F) -> search2(F, "test") end, S).

b3(S) ->
	lists:foreach(fun(F) -> search3(F, "test", "asdfa", "asdfasdfa", "asdfasfasfD", "adfasd", "asdfasdf", "asdfasdfas") end, S).

b4(S) ->
	lists:foreach(fun(F) -> search4(F, "test", "asdfa", "asdfasdfas", "asdfasdfasdf", "asdfasdf", "asdfasdfas", "asdfasfdasf") end, S).

b5(S) ->
	lists:foreach(fun(F) -> search5(F, "test", "asdfa", "asdfasdfa", "asdfasfasfD", "adfasd", "asdfasdf", "asdfasdfas") end, S).

%%%

search1(String, Sub) ->
	string:str(String, Sub).

search2(String, Sub) ->
	re:run(String, Sub).

search3(String, Sub1, Sub2, Sub3, Sub4, Sub5, Sub6, Sub7) ->
	string:str(String, Sub1),
	string:str(String, Sub2),
	string:str(String, Sub3),
	string:str(String, Sub4),
	string:str(String, Sub5),
	string:str(String, Sub6),
	string:str(String, Sub7).

search4(String, Sub1, Sub2, Sub3, Sub4, Sub5, Sub6, Sub7) ->
	re:run(String, Sub1 ++ "|" ++ Sub2 ++ "|" ++ Sub3 ++ "|" ++ Sub4 ++ "|" ++ Sub5 ++ "|" ++ Sub6 ++ "|" ++ Sub7).

search5(String, Sub1, Sub2, Sub3, Sub4, Sub5, Sub6, Sub7) ->
	(string:str(String, Sub1) > 0) orelse (string:str(String, Sub2) > 0)
	orelse(string:str(String, Sub3) > 0) orelse (string:str(String, Sub4) > 0)
	orelse(string:str(String, Sub5) > 0) orelse (string:str(String, Sub6) > 0)
	orelse(string:str(String, Sub7) > 0).

%% comprehensions better than map
%% foreach is best, for not save data

m1(S) ->
	lists:foreach(fun(F) -> F end, S).

m2(S) ->
	[X || X <- S].

m3(S) ->
	lists:map(fun(F) -> F end, S).

%%lists vs proplists

%L = [{a,b}, {c, d}]].
%lists:keyfind(a, 1, L).
%proplists:get_value(a, L).

%S = [ {X, 1} || X <- lists:seq(1, 1000)],
%%
%% 9> eff:start().
%% [{f1,8},{f2,137}]
%%
f1(S) ->
	lists:keyfind(999, 1, S).

f2(S) ->
	proplists:get_value(999, S).



%erl -pa log4erl/ebin/
% error_logger use io:format only disk cache
% 1. error_logger notify
% 3. log4erl

z1(S) ->
	application:start(sasl),
	error_logger:logfile({open , "/tmp/a1.log"}),
	lists:foreach(fun(Z) -> error_logger:info_msg("abc~n") end, lists:seq(1, 10000)),
	application:stop(sasl).

z2(S) ->
	{ok, Fd} = file:open("/tmp/a2.log", [write]),
	lists:foreach(fun(Z) -> file:write(Fd, "[warn] abc~n") end, lists:seq(1, 10000)),
	file:close(Fd).

z3(S) ->
	application:start(log4erl),
	log4erl:add_file_appender(file, {"/tmp", "a3", {size, 1000000}, 1, "log", info}),
	lists:foreach(fun(Z) -> log4erl:warn("abc~n") end, lists:seq(1, 10000)),
	application:stop(log4erl).

z4(S) ->
	{ok, Fd} = file:open("/tmp/a4.log", [write]),
	lists:foreach(fun(Z) -> io:format(Fd, "[warn] abc~n~n", []) end, lists:seq(1, 10000)),
	file:close(Fd).

z5(S) ->  
	application:start(log4erl),
    log4erl:conf("my.conf"),    
	    lists:foreach(fun(Z) -> log4erl:warn("abc~n") end, lists:seq(1, 10000)),
	application:stop(log4erl).

%%res tail

add1([]) -> [];  
add1([H|T]) -> [H+1|add1(T)].  
  
add2(R) -> add(R,[]).  
  
add3(R) -> lists:reverse(add(R,[])).  
  
add4([]) -> [];  
add4(L) -> lists:map(fun(X) -> X+1 end,L).  
  
add([],R) -> R;  
add([H|T],R) -> add(T,[H+1|R]).  
  
%t(N) ->  
%   L = lists:seq(1,N),  
%   {T1,_}=timer:tc(a,add1,[L]),  
%   {T2,_}=timer:tc(a,add2,[L]),  
%   {T3,_}=timer:tc(a,add3,[L]),  
%   {T4,_}=timer:tc(a,add4,[L]),  
%   io:format("~p ~p ~p ~p ~n",[T1,T2,T3,T4]).  

