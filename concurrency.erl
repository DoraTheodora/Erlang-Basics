%%%-------------------------------------------------------------------
%%% @author Joseph <joseph@josephdesktop>
%%% @copyright (C) 2021, Joseph
%%% @doc
%%%
%%% @end
%%% Created :  3 Mar 2021 by Joseph <joseph@josephdesktop>
%%%-------------------------------------------------------------------
%%% Commented by: Theodora Tataru

%!  COMMENT
%?  COMMAND
%*  OUTPUT

-module(concurrency). %! no capital letter names - it will think is a variable
%? everything that is public
-export([reverse/1, reverseAcc/2, reverseServer/0,revServer/1,replyServer/1,rpc/2,maxp/1,on_exit/2,keep_alive/2,sleep/1,flush_mailbox/0,priority_receive/0,smallfun/0]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%!  Basic Reverse function - reverseACC is not exported so is PRIVATE
%?  concurrency:reverseAcc([1,2,3,4,5,6],[]).
%*  [6,5,4,3,2,1]
%?  concurrency:reverseAcc([1,2,3,4,5,6],[fred, bert]).
%*  [6,5,4,3,2,1,fred,bert]

reverse(List)->
    reverseAcc(List,[]).
reverseAcc([],SoFar)->
    SoFar;
reverseAcc([Head|Tail],SoFar) ->
    reverseAcc(Tail,[Head|SoFar]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%!  Reverse running as a process
%?  Pid=spawn(concurrency,reverseServer,[]).
%*  <0.84.0> unique identifier
%?  PidA![1,2,3,4,5,6].
%*  Reversed list is:[6,5,4,3,2,1]
%*  [1,2,3,4,5,6]
%?  PidA![1,2,3,4,5,6,q,w,e,r,t,y,{a, b}].     
%*  Reversed list is:[{a,b},y,t,r,e,w,q,6,5,4,3,2,1]
%*  [1,2,3,4,5,6,q,w,e,r,t,y,{a,b}]

reverseServer()->
    receive
        List ->
            Answer=reverse(List),
            io:format("Reversed list is:~p~n",[Answer])
    end,
    reverseServer().


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%!  reverse server with a state
%?  PidB = spawn(concurrency, revServer, [0]).
%*  <0.89.0>
%?  PidB![1,2,3,4,5].
%*  List is:[5,4,3,2,1]  Called 1 times
%*  [1,2,3,4,5]
%?  PidB![1,2,3,4,5,q,w,e,r,t,y].
%*  List is:[y,t,r,e,w,q,5,4,3,2,1]  Called 2 times
%*  [1,2,3,4,5,q,w,e,r,t,y]

revServer(N)->
    receive
        List->
            Answer=reverse(List),
            io:format("List is:~p  Called ~p times~n",[Answer,N+1]),
            revServer(N+1)
end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%!  server that replies to sender
%!  will send a reply to a server
%!  "!" -> is sent
%?  Pid3=spawn(concurrency, replyServer, [0]).
%*  <0.93.0>
%?  Pid3!{self(), [1,2,3,4,5]}.
%*  List is:[5,4,3,2,1]  Called 1 times
%*  {<0.77.0>,[1,2,3,4,5]}
%?  Pid3!{self(), [1,2,3,4,5,a,b,c]}.
%*  List is:[c,b,a,5,4,3,2,1]  Called 2 times
%*  {<0.77.0>,[1,2,3,4,5,a,b,c]}

replyServer(N)->
    receive
        {Sender,List}->
            Answer=reverse(List),
            io:format("List is:~p  Called ~p times~n",[Answer,N+1]),
            Sender!{self(),Answer}, %! self()-> sender identifier -> will return the process id of the sender
            replyServer(N+1)
    end.    


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%!  Add RPC functionality
%!.............................................
%?  concurrency:rpc(Pid3,[1,2,3,4,5,6,7]).
%*  List is:[7,6,5,4,3,2,1]  Called 3 times
%*  [5,4,3,2,1] 
%!  this was waiting in the mail box
%?  concurrency:rpc(Pid3,[1,2,3,4,5,6,7]).
%*  List is:[7,6,5,4,3,2,1]  Called 4 times
%*  [c,b,a,5,4,3,2,1]
%?  concurrency:rpc(Pid3,[1,2,3,4,5,6,7]).
%*  List is:[7,6,5,4,3,2,1]  Called 5 times
%*  [7,6,5,4,3,2,1] 
%!  this was correct
%!.............................................
%?  Pid4=spawn(concurrency, replyServer, [0]).
%*  <0.106.0>
%?  concurrency:rpc(Pid4,[1,2,3,4,5,6,7]).    
%*  List is:[7,6,5,4,3,2,1]  Called 1 times
%*  [7,6,5,4,3,2,1]


rpc(Pid,Message)->
    Pid!{self(),Message},
    receive
        {Pid,Reply}->  %! read from the mail box
            Reply
end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%!  Maximum number of process than can be created
%!  Shows how much time it take to compute the answer
%?  concurrency:maxp(100000).
%*  Maximum allowed processes:262144
%*  Process spawn time=6.1 (5.0) microseconds
%*  ok

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
%!  sleep using timeout
%?  concurrency:sleep(10000). %sleep 10 seconds
%*  true

sleep(T)->
    receive
        after T -> %! if a response its not received after T seconds, do somethinf else
                true
        end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%!  empty the mail box
flush_mailbox()->
    receive _->
            flush_mailbox()
        after 0  ->
            true
        end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%!  receive prioriry from ~ nigel, after X seconds look at other mails
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


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%!  takes a list and turns it into an atom
%?  PidX=spawn(concurrency, smallfun, []).
%*  <0.1788.3>
%?  PidX![1,2,3,4,5].
%*  pid is:<0.1788.3>[1,2,3,4,5]
%*  my pid is:'\001\002\003\004\005'
%?  PidX![67,68,69].
%*  my pid is:<0.1788.3>"CDE"
%*  my pid is:'CDE'

smallfun()->
    receive
    X->
            io:format("my pid is:~p",[self()]),
            Result = list_to_atom(X),
            io:format("my pid is:~p~n",[Result])
    end,
    smallfun().


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%!  Fault tolerant Server
%!  Here we set up  our server to tell us when it fails
%!  monitors the PID 

on_exit(PID,FUN)-> %! PID ~ process id, FUN ~ is a function
    spawn(fun()-> Ref=monitor(process,PID), %! monitor the PID
                  receive
                      {'DOWN',Ref,process,PID,Reason}->FUN(Reason)  %! receive the crush: Down, refrence of the PID, the process and the reason
                  end                                               %! pass the reason to the FUN() function
          end).

%!  spawn a smallfun instance
%?  Pid=spawn(concurrency,smallfun,[]).
%!  attach instance to function using on_exit
%?  concurrency:on_exit(Pid,fun(Reason)->io:format("~p died because ~p",[Pid,Reason]) end).
%*  <0.1795.3>
%?  Pid![68,69,70].
%*  my pid is:<0.1792.3>"DEF"
%*  my pid is:'DEF'
%?  Pid!sdjhgdjsak.
%*  my pid is:<0.1792.3>sdjhgdjsak
%   <0.1792.3> died because {badarg,
%                            [{erlang,list_to_atom,[sdjhgdjsak],[]},
%                             {concurrency,smallfun,0,
%                                 [{file,"concurrency.erl"},{line,201}]}]}13> =ERROR REPORT==== 15-Mar-2021::16:37:23.745395 ===
%   Error in process <0.1792.3> with exit value:
%   {badarg,[{erlang,list_to_atom,[sdjhgdjsak],[]},
%         {concurrency,smallfun,0,[{file,"concurrency.erl"},{line,201}]}]}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%!  This server cannot die 
%?  concurrency:keep_alive(fred,fun()->concurrency:smallfun() end).
%?  concurrency:keep_alive(dora,fun()->concurrency:smallfun() end).
%*  <0.1803.3>
%?  dora![67,89].
%*  my pid is:<0.1802.3>"CY"
%*  my pid is:'CY'
%?  dora!dfdf.
%*  my pid is:<0.1802.3>dfdf
%   =ERROR REPORT==== 15-Mar-2021::16:44:54.038002 ===
%   Error in process <0.1802.3> with exit value:
%   {badarg,[{erlang,list_to_atom,[dfdf],[]},
%           {concurrency,smallfun,0,[{file,"concurrency.erl"},{line,201}]}]}
%!  but even if it crushed, it recreates the process with the same name -> look at the PID
%?  dora![67,89,100].                                              
%*  my pid is:<0.1806.3>"CYd"
%*  my pid is:'CYd'

keep_alive(Name,Fun)->
    Pid=spawn(Fun),     %! Pid with the function
    register(Name,Pid), %! give names to processes
    on_exit(Pid, fun(_Reason)->keep_alive(Name,Fun) end). %! when the process crushes, re-activates the process


%%%! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%! 
%%%!     HOW TO UPDATE A FUNCTION WITHOUT PUTTING DOWN THE SERVER
%%%!
%%%!     calculate(N)... bla bla bla
%%%!     primeServer(PrimeFunc)->
%%%!         receive
%%%!             {Sender, calculate, N} ->
%%%!                 Answer= calculate(N),
%%%!                 Sender!Answer,
%%%!                 primeServer(PrimeFunc);
%%%!             {Sender, update, BetterFunc} ->
%%%!                 primeServer{betterFunc}
%%%!     end.
%%%!
%%%?     F=spawn(server, primeServer, [fun(N)->calcPrime(N)end]).   -> initial function
%%%?     F!{self(), upDate, fun(N)->betterFunc(N)end}.              -> update function without the server being down
%%%! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%