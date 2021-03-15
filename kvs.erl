%%% @author Joseph <joseph@josephdesktop>
%%% @copyright (C) 2021, Joseph
%%% @doc
%%%
%%% @end
%%% Created : 15 Mar 2021 by Joseph <joseph@josephdesktop>

-module(kvs).

-export([start/0,store/2,lookup/1]).


start()->
    register(kvs,spawn(fun()->loop() end)).

store(Key,Value)->
    rpc({store,Key,Value}).


lookup(Key)->
    rpc({lookup,Key}).

rpc(Query)->
    kvs!{self(),Query},
    receive
        {kvs,Reply}->
            Reply
end.


loop()->
    receive
        {From,{store,Key,Value}}->
            put(Key,{ok,Value}),
            From!{kvs,true},
            loop();
        {From,{lookup,Key}} ->
            From!{kvs,get(Key)},
            loop()
end.

