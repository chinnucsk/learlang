-module(ponging).
-export([ping/2, pong/0, start/0, something/0]).

ping(0, PongPID) ->
    PongPID ! finished,
    io:format("quit pinging!\n");
ping(N, PongPID) ->
    PongPID ! {ping, self()},
    receive
        pong ->
            io:format("Pong received!\n")
    end,
    ping(N-1, PongPID).

pong() ->
    receive
        finished ->
            io:format("Stopping ponging\n");
        {ping, PingPID} ->
            io:format("Received ping!\n"),
            PingPID ! pong,
            pong()
    end.

start() ->
    PongPID = spawn(ponging, pong, []),
    spawn(ponging, ping, [3, PongPID]),
