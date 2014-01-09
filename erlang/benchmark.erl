-module(benchmark).
-author('cooldaemon@gmail.com').

-export([run/1]).

run(Count) ->
  Keys = lists:seq(1, Count),
  lists:foreach(
    fun ({F, TargetName}) ->
      {[SetRunTime, SetWallClock], [GetRunTime, GetWallClock]} = F(Keys),
      io:fwrite(
        "--<~s>--~nset:~p(~p)ms~nget:~p(~p)ms~n",
        [TargetName, SetRunTime, SetWallClock, GetRunTime, GetWallClock]
      )
    end,
    [
      {fun pd/1, "process dictionary"},
      {fun dict/1, "dict"},
      {fun ets/1,  "ets"},
      {fun gb_trees/1, "gb_trees"}
    ]
  ).

pd(Keys) ->
  {_SetResult, SetTimes} = benchmark(
    fun () -> lists:foreach(fun (N) -> put(N, a) end, Keys) end
  ),
  {_GetResult, GetTimes} = benchmark(
    fun () -> lists:map(fun (N) -> get(N) end, Keys) end
  ),
  {SetTimes, GetTimes}.

dict(Keys) ->
  {Dict, SetTimes} = benchmark(
    fun () ->
      lists:foldl(fun (N, D) -> D:store(N, a) end, dict:new(), Keys)
    end
  ),
  {_GetResult, GetTimes} = benchmark(
    fun () ->
      lists:map(fun (N) -> {ok, V} = Dict:find(N), V end, Keys)
    end
  ),
  {SetTimes, GetTimes}.

ets(Keys) ->
  Ets = ets:new(test_ets, [bag, private]),
  {_SetResult, SetTimes} = benchmark(
    fun () ->
      lists:foreach(fun (N) -> ets:insert(Ets, {N, a}) end, Keys)
    end
  ),
  {_GetResult, GetTimes} = benchmark(
    fun () ->
      lists:map(fun (N) -> [{_K, V}] = ets:lookup(Ets, N), V end, Keys)
    end
  ),
  {SetTimes, GetTimes}.

gb_trees(Keys) ->
  {Tree, SetTimes} = benchmark(
    fun () ->
      lists:foldl(
        fun (N, T) -> gb_trees:enter(N, a, T) end,
        gb_trees:empty(), Keys
      )
    end
  ),
  BalancedTree = gb_trees:balance(Tree),
  {_GetResult, GetTimes} = benchmark(
    fun () ->
      lists:map(fun (N) -> gb_trees:get(N, BalancedTree) end, Keys)
    end
  ),
  {SetTimes, GetTimes}.

benchmark(TargetFunction) ->
  lists:foreach(fun statistics/1, [runtime, wall_clock]),
  Result = TargetFunction(),
  Times = lists:map(
    fun (Type) -> {_, T} = statistics(Type), T end,
    [runtime, wall_clock]
  ),
  {Result, Times}.
