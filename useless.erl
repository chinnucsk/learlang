-module(useless).
-export([add/2, hello/0,greet_and_add_two/1,hello_to/2,len/1,tco_len/1]).

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
%% Examples taken from: http://learnyousomeerlang.com/recursion
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

%% Now, as all good programmers will know, 1 + len(T) will expand out
%% so that:
%% 1. len(T) = some number
%% 2. 1 + some number
%% 3. return some number (hence the full stop)
%%
%% This means that for large calls. Say, a list with a million zillion
%% items in it. Then you're going to be maintaining a stack through all
%% that.
%%
%% I.e.:
%%
%% len([1, 2, ... 1,000,000]) will call len 1,000,000 times before it
%% returns with 1,000,000. What's more, it will preserve the state in-
%% between those calls. So that the later values of len(T) can be pushed
%% up the stack towards those +1's.
%%
%% Enter tail-call-optimization.
%%
%% Since we don't really need to maintain the stack, because the operation
%% for what len(T) returns will always be the same (+1 to len(T)) then
%% we can have that operation be the final call.
%%
%% Below:
%%

%% Separate function which we export, simply passes along the call to
%% the internal implementation.
tco_len(L) ->
    tco_len(L,0).

%% same as before, essentially.
tco_len([], Val) ->
    Val;
%% since the last and only operation is the call to tco_len, Erlang can
%% throw away the stack since it will never be used again.
tco_len([_|T], Val) ->
    tco_len(T, Val+1).
