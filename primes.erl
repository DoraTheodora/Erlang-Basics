-module(primes).
-export([isPrime/1, allPrimes/1]).


allPrimes(N)->
    allPrimes(2,N,[]).

allPrimes(High, High, SoFar)->
    SoFar;
allPrimes(Low, High, SoFar)->
    LowIsPrime = isPrime(Low),
    if
        LowIsPrime == true->
            allPrimes(Low+1, High, [Low|SoFar]);
        true->
            allPrimes(Low+1, High, SoFar)
    end.

isPrime(2)->true;
isPrime(3)->true;
isPrime(5)->true;
isPrime(N)->
    noDivisible(2,N div 2+1, N).

noDivisible(A,A,Prime)->
    true;
noDivisible(Low, High, PossiblePrime) when PossiblePrime rem Low == 0 -> 
    false;
noDivisible(Low, High, PossiblePrime) -> 
    noDivisible(Low+1, High, PossiblePrime).


