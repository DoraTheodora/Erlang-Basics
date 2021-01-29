% Theodora Tataru
% C00231174
% Prime Numbers and Twin Prime Numbers

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
len([H|T])->
    1 + len(T).

%sum() returns the sum of all elements
sum([])->
    0;
sum([H|T])-> %_ instead of H -> used for something we do not use
    H + sum(T).

%product() returns the product of all elements
product([])->
    1;
product([H|T])->
    H * product(T).

%isInList() checks if an item is a list
isInList(_,[])->
    false;
isInList(Item, [Item|_])->
    true;
isInList(Item, [_|Tail])->
    isInList(Item, Tail).

%del() deletes an element from a list
del(Item, [])->
    [];
del(Item, [Item|Tail])->
    Tail;
del(Item, [Head|Tail])->
    [Head|del(Item, Tail)].


