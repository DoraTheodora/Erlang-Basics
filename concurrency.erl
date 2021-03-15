%%%-------------------------------------------------------------------
%%% @author Joseph <joseph@josephdesktop>
%%% @copyright (C) 2021, Joseph
%%% @doc
%%%
%%% @end
%%% Created :  3 Mar 2021 by Joseph <joseph@josephdesktop>
%%%-------------------------------------------------------------------

%! COMMENT
%? COMMAND
%* OUTPUT

-module(concurrency). %! no capital letter names - it will think is a variable
%? everything that is public
-export([reverse/1, reverseAcc/2, reverseServer/0,revServer/1,replyServer/1,rpc/2,maxp/1,on_exit/2,keep_alive/2,sleep/1,flush_mailbox/0,priority_receive/0,smallfun/0]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%! Basic Reverse function - reverseACC is not exported so is PRIVATE
%? concurrency:reverseAcc([1,2,3,4,5,6],[]).
%* [6,5,4,3,2,1]
%? concurrency:reverseAcc([1,2,3,4,5,6],[fred, bert]).
%* [6,5,4,3,2,1,fred,bert]

reverse(List)->
    reverseAcc(List,[]).
reverseAcc([],SoFar)->
    SoFar;
reverseAcc([Head|Tail],SoFar) ->
    reverseAcc(Tail,[Head|SoFar]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%! Reverse running as a process
%? Pid=spawn(concurrency,reverseServer,[]).
%* <0.84.0> unique identifier
%? PidA![1,2,3,4,5,6].
%* Reversed list is:[6,5,4,3,2,1]
%* [1,2,3,4,5,6]
%? PidA![1,2,3,4,5,6,q,w,e,r,t,y,{a, b}].     
%* Reversed list is:[{a,b},y,t,r,e,w,q,6,5,4,3,2,1]
%* [1,2,3,4,5,6,q,w,e,r,t,y,{a,b}]

reverseServer()->
    receive
        List ->
            Answer=reverse(List),
            io:format("Reversed list is:~p~n",[Answer])
    end,
    reverseServer().


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%! reverse server with a state
%? PidB = spawn(concurrency, revServer, [0]).
%* <0.89.0>
%? PidB![1,2,3,4,5].
%* List is:[5,4,3,2,1]  Called 1 times
%* [1,2,3,4,5]
%? PidB![1,2,3,4,5,q,w,e,r,t,y].
%* List is:[y,t,r,e,w,q,5,4,3,2,1]  Called 2 times
%* [1,2,3,4,5,q,w,e,r,t,y]

revServer(N)->
    receive
        List->
            Answer=reverse(List),
            io:format("List is:~p  Called ~p times~n",[Answer,N+1]),
            revServer(N+1)
end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%! server that replies to sender
%! will send a reply to a server
%! "!" -> is sent
%? Pid3=spawn(concurrency, replyServer, [0]).
%* <0.93.0>
%?  Pid3!{self(), [1,2,3,4,5]}.
%* List is:[5,4,3,2,1]  Called 1 times
%* {<0.77.0>,[1,2,3,4,5]}
%? Pid3!{self(), [1,2,3,4,5,a,b,c]}.
%* List is:[c,b,a,5,4,3,2,1]  Called 2 times
%* {<0.77.0>,[1,2,3,4,5,a,b,c]}

replyServer(N)->
    receive
        {Sender,List}->
            Answer=reverse(List),
            io:format("List is:~p  Called ~p times~n",[Answer,N+1]),
            Sender!{self(),Answer}, %! self()-> sender identifier -> will return the process id of the sender
            replyServer(N+1)
    end.    


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%! Add RPC functionality
%!.............................................
%? concurrency:rpc(Pid3,[1,2,3,4,5,6,7]).
%* List is:[7,6,5,4,3,2,1]  Called 3 times
%* [5,4,3,2,1] 
%! this was waiting in the mail box
%? concurrency:rpc(Pid3,[1,2,3,4,5,6,7]).
%* List is:[7,6,5,4,3,2,1]  Called 4 times
%* [c,b,a,5,4,3,2,1]
%? concurrency:rpc(Pid3,[1,2,3,4,5,6,7]).
%* List is:[7,6,5,4,3,2,1]  Called 5 times
%* [7,6,5,4,3,2,1] 
%! this was correct
%!.............................................
%? Pid4=spawn(concurrency, replyServer, [0]).
%* <0.106.0>
%? concurrency:rpc(Pid4,[1,2,3,4,5,6,7]).    
%* List is:[7,6,5,4,3,2,1]  Called 1 times
%* [7,6,5,4,3,2,1]


rpc(Pid,Message)->
    Pid!{self(),Message},
    receive
        {Pid,Reply}->  %! read from the mail box
            Reply
end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%! Maximum number of process than can be created
%! Shows how much time it take to compute the answer
%? concurrency:maxp(100000).
%* Maximum allowed processes:262144
%* Process spawn time=6.1 (5.0) microseconds
%* ok

maxp(N)->
    Max=erlang:system_info(process_limit),
    io:format("Maximum allowed processes:~p~n",[Max]),
    statistics(runtime),
    statistics(wall_clock),
    L=for(1,N,fun()->spawn(fun()->wait()end)end), %! for loop
    {_,Time1}=statistics(runtime),
    {_,Time2}=statistics(wall_clock),
    lists:foreach(fun(Pid)->Pid!die end,L), %! deletes each process
    U1=Time1*1000/N,
    U2=Time2*1000/N,
    io:format("Process spawn time=~p (~p) microseconds~n",[U1,U2]).

wait()->
    receive
        die-> void
    end.
for(N,N,F)->
    [F()]; %! F() variable containing a function
for(I,N,F) ->
    [F()|for(I+1,N,F)].

    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%! sleep using timeout

sleep(T)->
    receive
        after T ->
                true
        end.

flush_mailbox()->
    receive _->
            flush_mailbox()
        after 0  ->
            true
        end.

priority_receive()->
    receive
        {nigel,X}->
            {nigel,X}
    after 0 ->
        receive
            Any->
                Any
        end
    end.



smallfun()->
    receive
    X->
            list_to_atom(X),
            io:format("my pid is:~p",[self()])
    end,
    smallfun().


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %! Fault tolerant Server
 %! Here we set up  our server to tell us when it fails
on_exit(PID,FUN)->
    spawn(fun()-> Ref=monitor(process,PID),
                  receive
                      {'DOWN',Ref,process,PID,Reason}->FUN(Reason)
                  end
          end).
%! spawn a smallfun instance
%! Pid=spawn(concurrency,smallfun,[]).
%! attach instance to function using on_exit
%! concurrency:on_exit(Pid,fun(Reason)->io:format("~p died because ~p",[Pid,Reason]) end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%! This server cannot die 
%! concurrency:keep_alive(fred,fun()->concurrency:smallfun() end).

keep_alive(Name,Fun)->
    Pid=spawn(Fun),
    register(Name,Pid),
    on_exit(Pid, fun(_Reason)->keep_alive(Name,Fun) end).


