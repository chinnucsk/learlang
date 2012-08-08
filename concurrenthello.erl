-module(concurrenthello).
-export([say/2, start/0]).

say(_, 0) ->
    done;
say(What, Times) ->
    io:format("~p~n", [What]),
    say(What, Times - 1).

start() ->
    spawn(concurrenthello, say, [hello, 3]),
    spawn(concurrenthello, say, [goodbye, 3]).
