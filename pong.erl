%%%-------------------------------------------------------------------
%%% @author Joseph <joseph@joseph-OptiPlex-5050>
%%% @copyright (C) 2020, Joseph
%%% @doc
%%%
%%% @end
%%% Created :  3 Feb 2020 by Joseph <joseph@joseph-OptiPlex-5050>
%%%-------------------------------------------------------------------
-module(pong).
-export([ping/1,pong/1]).

ping(N)->
    receive
	{ping,Sender}->
	    io:format("ping number: ~p from ~p",[N,Sender]), %print
	    Sender!{pong,self()}, %sender ID - self() is a function-> process ID
	    ping(N+1)
    end.

pong(N)->
    receive
	{pong,Sender}->
	    io:format("pong number: ~p from ~p",[N,Sender]), %print
	    Sender!{ping,self()},
	    pong(N+1)
    end.

%! ~p -> %d from t, to get the variables
%!  ! -> used to send a message
%? c(pong).
%? PongId=spawn(pong,pong,[0]). -> (name of the file, name of the function, a list of all paramaters)
%? PingId=spawn(pong,ping,[0]).
%* <0.87.0> -> is the function ID
%? PingId!{ping,self()}. 
%* ping number: 0 from <0.77.0>{ping,<0.77.0>}
%? PingId!{ping,PongId}. %? Goes forever!

