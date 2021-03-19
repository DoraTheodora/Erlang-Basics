%%% @author Joseph <joseph@josephdesktop>
%%% @copyright (C) 2021, Joseph
%%% @doc
%%%
%%% @end
%%% Created : 15 Mar 2021 by Joseph <joseph@josephdesktop>

-module(kvs).

-export([start/0,store/2,lookup/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%!  spawn the loop() function ~ loop() runs ina process
%!  returns a PID

start()->
    %?  Pid=spawn(fun()-> loop()end),
    %?  register(joe, Pid),
    %!  This lines are combined in the following line
    register(kvs,spawn(fun()->loop() end)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
store(Key,Value)->
    rpc({store,Key,Value}).


lookup(Key)->
    rpc({lookup,Key}).

%! is a procedure call - rpc hides the receive block, it still needs to wait for the reply, but is not shown
rpc(Query)->
    kvs!{self(),Query},
    receive
        {kvs,Reply}->
            Reply
%! without the after waits forever
%! after 10000 ->
%!    {error, false}
end.

%! 
loop()->
    receive
        {From,{store,Key,Value}}->  %! recieves a dictionary
            put(Key,{ok,Value}),    %! adds to the dictionaty the tuple
            From!{kvs,true},        %! From = sender, sending from sender, from my address
            loop();
        {From,{lookup,Key}} ->
            From!{kvs,get(Key)},    %! sends the key value from dictionary to the receiver
            loop()
end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%? kvs:start().
%%%? 30> kvs:store(dora, cool).
%%%? true
%%%? 31> kvs:store(mihai, better).
%%%? true
%%%? 32> kvs:lookup(dora).
%%%?{ok,cool}
%%%? 33> kvs:lookup(mihai).
%%%? {ok,better}

%%*  Terminal 1 (dora@md)                    %%*  Terminal 2 mihai@md)
%%!  erl -sname mihai                        %%!  erl -sname mihai
%%!  kvs:lookup(dora).                       %%!  rpc:call(dora@md, kvs, store, [dora, {cool, emacs}]).
