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

        {login, Return, From, Who} ->
            case orddict:find(Who, UserList) of
                {ok, _ } ->
                    Return ! {error, "Already logged in"},
                    server(UserList);
                error ->
                    Return ! {ok, logged_in},
                    server(orddict:store(Who, From, UserList))
            end;

        {logout, Return, Who} ->
            case orddict:find(Who, UserList) of
                {ok, _ } ->
                    Return ! {ok, logged_out},
                    server(orddict:erase(Who, UserList));
                error  ->
                    Return ! {error, "You were not logged in!"},
                    server(UserList)
            end

    after
        10000 ->
            io:format("Polling.....~n"),
            server(UserList)
    end.

%% Logs a user onto the server.
login(Username) ->
    % starts a client receive channel.
    {server, ?SERVER} ! {login, self(), node(), Username},

    receive
        {ok, logged_in} ->
            io:format(logged_in),
            register(getmsg, spawn(chatapp, receiver, []));
        {error, Message} ->
            io:format(Message)
    after
        3000 ->
            io:format("Error logging in")
    end,
    ok.

%% Logs a user out on the server.
logout(Username) ->
    {server, ?SERVER} ! {logout, self(), Username},
    receive
        {ok, logged_out} ->
            io:format(logged_out),
            exit(whereis(getmsg), kill);
        {error, Message} ->
            io:format(Message)
    end,
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
