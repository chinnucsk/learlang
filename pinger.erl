-module(pinger).
-export([start_pong/0, ping/1, pong/0]).

ping(Node) ->
    {ponger, Node} ! {ping, self()},
    receive
        pong ->
            io:format("")
    after
        1000 ->
            io:format("No response!")
    end.

pong() ->
    receive
        {ping, PID} ->
            io:format("Ping:~p~n", [PID]),
            PID ! pong,
            pong()
    end.

start_pong() ->
    register(ponger, spawn(pinger, pong, [])).
