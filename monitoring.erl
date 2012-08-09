-module(monitoring).
-export([process/0,goRun/0,new/0]).

process() ->
    receive
        _ ->
            ok
    after
        5000 ->
            io:format("TIMED OUT~n"),
            1/0
    end.

goRun() ->
    spawn(?MODULE, new, []).

new() ->
    process_flag(trap_exit, true),
    Pid = spawn_link(?MODULE, process, []),
    receive
        {'EXIT', Pid, _ } ->
            io:format("CRASHED OH NOES!~n"),
            io:format("Restarting process....~n"),
            new()
    end.
