-module(gb_tree_cache).
-behaviour(gen_server).
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
-define(SERVER, ?MODULE).
-export([set/2, set/3, get/1, del/1]).
 
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
 
set(K, V, T) ->
    gen_server:call(?SERVER, {set, K, V, T}).
 
set(K, V) ->
    gen_server:call(?SERVER, {set, K, V}).
 
get(K) ->
    gen_server:call(?SERVER, {get, K}).
 
del(K) ->
    gen_server:call(?SERVER, {del, K}).
 
init([]) ->
    {ok, gb_trees:empty()}.
 
handle_call({set, K, V, T}, _From, Tree) ->
    case gb_trees:lookup(K, Tree) of
        none ->
            none;
        {value, {V, forever}} ->
            none;
        {value, {V, OldTRef}} ->
            timer:cancel(OldTRef)
    end,
    {ok, TRef} = timer:apply_after(T * 1000, ?MODULE, del, [K]),
    Tree2 = gb_trees:enter(K, {V, TRef}, Tree),
    {reply, {ok, K, V}, Tree2};
 
handle_call({set, K, V}, _From, Tree) ->
    case gb_trees:lookup(K, Tree) of
        none ->
            none;
        {value, {V, forever}} ->
            none;
        {value, {V, OldTRef}} ->
            timer:cancel(OldTRef)
    end,
    Tree2 = gb_trees:enter(K, {V, forever}, Tree),
    {reply, {ok, K, V}, Tree2};
 
handle_call({get, K}, _From, Tree) ->
    Reply = case gb_trees:lookup(K, Tree) of
                none ->
                    none;
                {value, {Value, _}} ->
                    {ok, Value}
            end,
    {reply, Reply, Tree};
 
handle_call({del, K}, _From, Tree) ->
    Tree2 = gb_trees:delete_any(K, Tree),
    {reply, {ok, K}, Tree2}.
 
handle_cast(_Msg, State) ->
    {noreply, State}.
 
handle_info(_Info, State) ->
    {noreply, State}.
terminate(_Reason, _State) ->
    ok.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

