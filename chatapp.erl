-module(chatapp).
-author("Aaron France").
-export([startServer/0,sendMessage/2,server/1,login/1,logout/1,receiver/0]).
-define(SERVER, server@FRANCEA1).

%% starts the server and registers it's Pid to server.
startServer() ->
    register(server, spawn(chatapp, server, [orddict:new()])),
    ok.

%% recursive loop which constantly reads it's mailbox to find
%% instructions from clients.
%%
%% We can deal with handling messages to clients and logging
%% in and out.
server(UserList) ->

    receive
        {message, From, To, Message} ->
            case orddict:find(To, UserList) of
                {ok, _ } ->
                    io:format("Message for: ~p. With: ~p~n", [To, Message]),
                    {getmsg, orddict:fetch(To, UserList)} ! {message, Message},
                    server(UserList);
                error ->
                    {getmsg, From} ! {message, "Recipient not found!"},
                    server(UserList)
            end;
        
        {login, From, Who} ->
            {getmsg, From} ! {message, logged_in},
            server(orddict:store(Who, From, UserList));
        
        {logout, From, Who} ->
            {getmsg, From} ! {message, logged_out},
            server(orddict:erase(Who, UserList))
    
    after
        10000 ->
            io:format("Polling.....~n"),
            server(UserList)
    end.

%% Logs a user onto the server.
login(Username) ->
    % starts a client receive channel.
    register(getmsg, spawn(chatapp, receiver, [])),
    {server, ?SERVER} ! {login, node(), Username},
    ok.

%% Logs a user out on the server.
logout(Username) ->
    {server, ?SERVER} ! {logout, node(), Username},
    exit(whereis(getmsg), kill),
    ok.

%% Executes the receive loop. This call will block so use the login
%% which executes this function asynchronously.
receiver() ->
    % recursive loop which constantly reads it's mail box to find
    % messages to write to stdout.
    receive
        {message, Message} ->
            io:format("~p~n", [Message]),
            receiver()
    end.

%% Sends Message towards To via the server.
sendMessage(To, Message) ->
    % send a message to the server which is intended for a specific
    % recipient.
    {server, ?SERVER} ! {message, node(), To, Message},
    ok.
