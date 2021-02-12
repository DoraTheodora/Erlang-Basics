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

zipFlattenList([],[])->
    [];
zipFlattenList([],List)->
    List;
zipFlattenList(List, [])->
    List;
zipFlattenList([H1|T1], [H2|T2])->
    [H1,H2|zipFlattenList(T1,T2)].

% * flatten() gives back from nested lists a flatten list [a,[1,2,3,[a,b,c]]] => [a,1,2,3,a,b,c]
flatten([])->
    [];
flatten([[]|List])->
    flatten(List);
flatten([[H1|T1]|T2])->
    flatten([H1|T1])++flatten(T2);
flatten([H|T])->
    [H|flatten(T)].

% * reverse() gives back the list in reverse order [1,2,3] => [3,2,1]
rev([],ReverseList)->
    ReverseList;
rev([H|T],ReverseList)->
    rev(T,[H|ReverseList]).

revs([])->
    [];
revs([H|T])->
    revs(T)++[H].

% * insertAtPosition() inserts an element at a certain pisition x,3,[1,2,3,4,5,6] => [1,2,3,x,4,5,6]
insertAtPosition(Item, 0, List)->
    [Item|List];
insertAtPosition(Item, Pos, [H|T])->
    [H|insertAtPosition(Item, Pos-1, T)].   

% * sortList() returns the list sorted
sortList([])->
    [];
sortList([H|T])->
    % ? [everything less then H]++[H]++[everything grater than H]
    sortList([X||X<-T,X<H])++[H]++sortList([Y||Y<-T,Y>H]).

    
% * factorial() gives back a factorial result from N
factorial(0) ->
    1;
factorial(N)->
    N*factorial(N-1).

% TODO: sortList() sorts the list 

% * for(start, end, function)
for(Start, Start, Function)->
    [];
for(Start, End, Function)->
    Function(Start), 
    [Function(Start)|for(Start+1, End, Function)].




