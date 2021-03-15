-module(distribution).
-compile(export_all).


reverse(List)->
    reverseAcc(List, []).

reverseAcc([], SoFar)->
    SoFar;
reverseAcc([Head|Tail], SoFar)->
    reverseAcc(Tail,[Head|SoFar]).

reverseServer()->
    receive
        List->
            Answer=reverse(List),
            io:format("Reverse list is: ~p", [Answer]),
            reverseServer()
    end.

revServer(N)->
    receive
        List->
            Answer=reverse(List),
            io:format("reversed list is: ~p, called ~p times", [Answer, N+1]),
            revServer(N+1)
    end.

replyServer(N)->
    receive
        {SenderPid, List}->
            Answer=reverse(List),
            SenderPid!{self(),Answer, N+1},
            replyServer(N+1)
    end.

rpc(Pid,Message)->
    Pid!{self(), Message},
    receive
        {Pid,Reply,N}->
            Reply
    end.

%? Compile: c(main).
%! Run: main:functionName(parameters).
%? Pid=spawn(distribution,replyServer,[0]).
%? Pid![1,2,3]. 
%! variable=spawn(program, server, parameters).
%* <0.90.0>
%? distribution:rpc(Pid,[1,2,3,4,5]).          
%* [5,4,3,2,1]