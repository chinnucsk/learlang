-module(chatapp).
-export([startServer/0,sendMessage/2,server/0,startReceiver/0,receiver/0]).

startServer() ->
    % starts the server and registers it's handle.
    register(server, spawn(chatapp, server, [])),
    ok.

server() ->
    % recursive loop which constantly reads it's mailbox to find
    % messages to send to clients.
    receive
        {message, PID, To,Message} ->
            io:format("Message for: ~p. With: ~p~n", [PID, Message]),
            {receiver, To} ! {message, Message},
            server()
    end.

startReceiver() ->
    % starts a client receive channel.
    register(receiver, spawn(chatapp, receiver, [])),
    ok.

receiver() ->
    % recursive loop which constantly reads it's mail box to find
    % messages to write to stdout.
    receive
        {message, Message} ->
            io:format("~p~n", [Message]),
            receiver()
    end.

sendMessage(Message, To) ->
    % send a message to the server which is intended for a specific
    % recipient.
    {server, meme@FRANCEA1} ! {message, self(), To, Message},
    ok.
