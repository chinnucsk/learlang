-module(ponging).
-export([start_ping/1, start_pong/0, ping/2, pong/0]).

ping(0, Node) ->
    {pong, Node} ! finished,
    io:format("ping finished~n", []);

ping(N, Node) ->
    {pong, Node} ! {ping, self()},
    receive
        pong ->
            io:format("Ping received pong~n", [])
    end,
    ping(N - 1, Node).

pong() ->
    receive
        finished ->
            io:format("Pong finished~n", []);
        {ping, Ping_PID} ->
            io:format("Pong received ping~n", []),
            Ping_PID ! pong,
            pong()
    end.

start_pong() ->
    register(pong, spawn(ponging, pong, [])).

start_ping(Pong_Node) ->
    spawn(ponging, ping, [3, Pong_Node]).
