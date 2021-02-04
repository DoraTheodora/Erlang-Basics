% Theodora Tataru
% C00231174
% Prime Numbers and Twin Prime Numbers

% ! COmpile: c(main).
% ! Run: main:functionName(parameters).

-module(main).
-compile(export_all).

helloWorld()->
    io:fwrite("HelloWorld").

% A list is either:
% Emphty
% or contains one item followed by a list [X|List]
% [1,2,3]
% [1 |[2,3]]
% [1 | 2 | [3] | []]

%length() returns length of list
len([])->
    0;
len([_|T])->
    1 + len(T).

%sum() returns the sum of all elements
sum([])->
    0;
sum([H|T])-> % ? use _ instead of H -> used for something we do not use
    H + sum(T).

% * product() returns the product of all elements
product([])->
    1;
product([H|T])->
    H * product(T).

% * isInList() checks if an item is a list
isInList(_,[])->
    false;
isInList(Item, [Item|_])->
    true;
isInList(Item, [_|Tail])->
    isInList(Item, Tail).

% * del() deletes an element from a list
del(Item, [])->
    [];
del(Item, [Item|Tail])->
    Tail;
del(Item, [Head|Tail])->
    [Head|del(Item, Tail)].

% * insertAtFront() inserts an element at the front of the list
insertAtFront(Element, [])->
    [Element];
insertAtFront(Element,[H|T])->
    [Element|insertAtFront(H, T)].

insertFront(Item,List)->
    [Item|List].

% * appendAtEnd() inserts and element at the end of the list
appendAtEnd(Element, [])->
    [Element];
appendAtEnd(Element, [H|T])->
    [H|appendAtEnd(Element, T)].

% * concatonateTwoLists() concatonates two lists together
concatonateTwoLists([], [])->
    [];
concatonateTwoLists([], [H|T])->
    [H|T];
concatonateTwoLists([H|T], [])->
    [H|T];
concatonateTwoLists(L1,[H|T])->
    %[[A|concatonateTwoLists(B, [])] | [H|concatonateTwoLists([],T)]].
    concatonateTwoLists(appendAtEnd(H, L1), T).

conc([], [])->
    [];
conc([], [H|T])->
    [H|T];
conc([H|T], [])->
    [H|T];
conc([H1|T1], [H2|T2])->
    [H1|conc(T1, [H2|T2])].

% * zip() zips to list together => [1,2,3] and [a,b,c] => [1,a,2,b,3,c]
zipTwoLists([],[])->
    [];
zipTwoLists([H1|T1], [H2|T2])->
    [{H1, H2}|zipTwoLists(T1, T2)].
    
% * factorial() gives back a factorial result from N
factorial(0) ->
    1;
factorial(N)->
    N*factorial(N-1).

% TODO: insertAtPosition() inserts an element at a certain pisition
% TODO: sortList() sorts the list 
% TODO: reverseList() reverses a list
% TODO: flatten([[1,2,[3]], 4,5, [[[6]]]) == [1,2,3,4,5,6]



% * for(start, end, function)
for(Start, Start, Function)->
    [];
for(Start, End, Function)->
    Function(Start), 
    [Function(Start)|for(Start+1, End, Function)].




