-module(useless).
-export([add/2, hello/0,greet_and_add_two/1,hello_to/2,len/1]).

add(A,B)->
    A + B.

hello()->
    io:format("Hello world!\n").

greet_and_add_two(X) ->
    hello(),
    add(X,2).


%% Instead of if/else chains, or switches we can have pattern matching
%%
%% male/female are atoms and are valid identifiers which 'resolve' to
%% themselves. They are what they say they are.
%%
%% In the match (male, Name) male is an atom and Name is a variable to
%% which anything can be bound. Thus, (male, "Aaron") will return:
%%     Hello, Mr. Aaron
%%
hello_to(male, Name) ->
    io:format("Hello, Mr. ~s", [Name]);
hello_to(female, Name) ->
    io:format("Hello, Ms/Mrs ~s", [Name]);
hello_to(_, Name) ->
    io:format("WATUP DAWG ~s", [Name]).


%% !Very interesting!
%% 
%% len([]) matches on an empty list whereas len([_|T]) matches on a
%% list with =>1 element it. To which it returns 1+len(T).
%%
%% [_|T] throws away the head of the list, and binds the tail of the
%% list to T.
%%
%% What this gives is a recusive loop through the list splitting off
%% the head of the list each time and adding one to the length of the
%% tail.
len([]) ->
    0;
len([_|T]) ->
    1 + len(T).
