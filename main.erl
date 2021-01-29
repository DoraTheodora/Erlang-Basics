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

% * appendAtEnd() inserts and element at the end of the list
appendAtEnd(Element, [])->
    [Element];
appendAtEnd(Element, [H|T])->
    [H|appendAtEnd(Element, T)].

concatonateTwoLists([], [])->
    [];
concatonateTwoLists([], [H|T])->
    [H|T];
concatonateTwoLists([H|T], [])->
    [H|T];
concatonateTwoLists(L1,[H|T])->
    %[[A|concatonateTwoLists(B, [])] | [H|concatonateTwoLists([],T)]].
    concatonateTwoLists(appendAtEnd(H, L1), T).
    
% TODO: insertAtFront(q, [1,2,3]) == [q,1,2,3]
% TODO: appeend(q, [1,2,3]) == [1,2,3,q]
% TODO: concatonate ([1,2,3], [a,b,c]) == [1,2,3,a,b,c]
% TODO: zip([1,2,3].[a,b,c]) == [1,a,2,b,3,c]
% TODO: flatten([[1,2,[3]], 4,5, [[[6]]]) == [1,2,3,4,5,6]



% * for(start, end, function)
for(Start, Start, Function)->
    [];
for(Start, End, Function)->
    Function(Start), 
    [Function(Start)|for(Start+1, End, Function)].




