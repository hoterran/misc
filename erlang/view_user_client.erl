#!/usr/local/bin/escript
%%! -name rds_client@127.0.0.1 -setcookie rdstest@jq
 
%-module(view_user_client).
%-compile(export_all).
 
%% Arg:
%%      ./view_user_client node cookie user
%%    
%% Use:
%%      ./view_user_client rdstest@10.1.147.7 rdstest@jq jc200
%%
%% Dont Execute Frequently
%%
 
main(Arg) ->
    [ConnectNode, Cookie, ViewUser] = Arg,
    %io:format("~p ~p ~p ~p ~p ~n", [node(), ConnectNode, Cookie, ViewUser, nodes()]),
    erlang:set_cookie(node(), list_to_atom(Cookie)),
    net_adm:ping(list_to_atom(ConnectNode)),
 
    lists:foreach(fun(Node) ->
    [io:format("~p, ~p ~n", [gen_fsm:sync_send_all_state_event(Pid, client_ip), User]) 
        || {Pid, User} <- rpc:call(Node, ets, tab2list, [state_machines]), User =:= ViewUser]
    end, nodes()).

