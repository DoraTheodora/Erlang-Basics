-module(primes).
-export([allPrimes/1,isPrime/1,start/0, rpcFred/1]).


findPrimes()->
    receive
        {Sender,N} ->
            Answer=allPrimes(N),
            io:format("Sender is:~p~n",[self()]),
            Sender!{self(),Answer}
    end.


start()->
    register(fred,Pid=spawn(fun()->findPrimes() end)).

rpcFred(N)->
    io:format("I am ~p~n",[self()]),
    fred!{self(),N},
    receive
        {fred,Answer}->
            io:format("There are ~d primes less than ~d~n",[Answer,N])
end.

allPrimes(N)->
    allPrimes(2,N,0).
allPrimes(High, High, NoOfPrimesFound)->
    NoOfPrimesFound;
allPrimes(Low, High, TotalSoFar)->
    LowIsPrime=isPrime(Low,0),
    if
        LowIsPrime==true ->
            put(TotalSoFar+1,Low),
            allPrimes(Low+1,High,TotalSoFar+1);
        true ->
            allPrimes(Low+1,High,TotalSoFar)
    end.
            
%find all primes less than N
%allPrimes(N)->
%    allPrimes(2,N,[]).

%allPrimes(High,High,SoFar)->
%    length(SoFar);
%allPrimes(Low,High,SoFar) ->
%    LowIsPrime=isPrime(Low,SoFar),
%    if
%        LowIsPrime==true ->
%            allPrimes(Low+1,High,append(Low,SoFar));
%        true ->
%            allPrimes(Low+1,High,SoFar)
%    end.

isPrime(N, PrimesChecked)->
    isPrime(N, math:sqrt(N),PrimesChecked).
isPrime(N,Max,PrimesChecked)->
    NextPrime = get(PrimesChecked+1),
    if 
        NextPrime> Max ->
            true;
        N rem NextPrime == 0 ->
            false;
        true -> isPrime(N, Max, PrimesChecked+1)
    end.



%isPrime(N,Max,[Head|Tail]) when Head > Max ->true; %isPrime(N,Tail);
%isPrime(N,Max,[First|Rest]) when N rem First==0->
%    false;
%isPrime(N,Max,[_|Rest]) ->
%    isPrime(N,Max,Rest).


%isPrime(N,PrimeList)->isPrime(N,math:sqrt(N),PrimeList).
%isPrime(Number,ListOfPrimes)
%isPrime(N,Max,[])->
%    true;
%isPrime(N,Max,[Head|Tail]) when Head > Max ->true; %isPrime(N,Tail);
%isPrime(N,Max,[First|Rest]) when N rem First==0->
%    false;
%isPrime(N,Max,[_|Rest]) ->
%    isPrime(N,Max,Rest).

%return true if N is a prime 
%prime if not evenly divisible by any number less than N
isPrime(2)->true;
isPrime(3)->true;
isPrime(N) when N rem 2 == 0 ->false;
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

%? primer!{self(), 2343434}.