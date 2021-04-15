%%% @author Joseph <joseph@josephdesktop>
%%% @copyright (C) 2021, Joseph
%%% @doc
%%%
%%% @end
%%% Created : 25 Mar 2021 by Joseph <joseph@josephdesktop>

-module(primes).
-export([allPrimes/1,isPrime/1]).


findPrimes()->
    receive
        {Sender,N} ->
            Answer=allPrimes(N),
            Sender!{self(),Answer}
    end.


start()->
    register(fred,Pid=spawn(fun()->findPrimes() end)).

rpcFred(N)->
    fred!{self(),N},
    receive
        {fred,Answer}->
            io:format("There are ~d primes less than ~d~n",[Answer,N])
end.
            
%find all primes less than N
allPrimes(N)->
    allPrimes(2,N,[]).

allPrimes(High,High,SoFar)->
    length(SoFar);
allPrimes(Low,High,SoFar) ->
    LowIsPrime=isPrime(Low,SoFar),
    if
        LowIsPrime==true ->
            allPrimes(Low+1,High,append(Low,SoFar));
        true ->
            allPrimes(Low+1,High,SoFar)
    end.

isPrime(N,PrimeList)->isPrime(N,math:sqrt(N),PrimeList).
%isPrime(Number,ListOfPrimes)
isPrime(N,Max,[])->
    true;
isPrime(N,Max,[Head|Tail]) when Head > Max ->true; %isPrime(N,Tail);
isPrime(N,Max,[First|Rest]) when N rem First==0->
    false;
isPrime(N,Max,[_|Rest]) ->
    isPrime(N,Rest).





%return true if N is a prime 
%prime if not evenly divisible by any number less than N
isPrime(2)->true;
isPrime(3)->true;
isPrime(N) when N rem 2 ==0 ->false;
isPrime(N) when N rem 3 == 0 ->false;
isPrime(N)->
    notDivisible(3,N div 2 + 1,N).
%notDivisible(Start,End,PossiblePrime)->    
notDivisible(Low,High,Prime) when Low >=High ->
    true;
notDivisible(Low,High,PossiblePrime) when PossiblePrime rem Low==0-> 
    false;
notDivisible(Low,High,PossiblePrime)->
    notDivisible(Low+2,High,PossiblePrime).

append(X,[])->
    [X];
append(X,[H|T]) ->
    [H|append(X,T)].
