-module(chatapp).
-author("Aaron France").
-export([startServer/0,sendMessage/2,server/2,login/1,logout/1,receiver/0]).
-define(SERVER, server@FRANCEA1).

%% starts the server and registers it's Pid to server.
startServer() ->
    register(server, spawn(chatapp, server, [orddict:new(),orddict:new()])),
    ok.

%% recursive loop which constantly reads it's mailbox to find
%% instructions from clients.
%%
%% We can deal with handling messages to clients and logging
%% in and out.
server(UserListA, UserListB) ->

    receive
        {status, Return, From} ->
            Return ! orddict:find(From, UserListB),
            server(UserListA, UserListB);

        {message, From, To, Message} ->
            case orddict:find(To, UserListA) of
                {ok, _ } ->
                    io:format("Message for: ~p. With: ~p~n", [To, Message]),
                    {getmsg, orddict:fetch(To, UserListA)} ! {message, Message},
                    server(UserListA, UserListB);
                error ->
                    {getmsg, From} ! {message, "Recipient not found!"},
                    server(UserListA, UserListB)
            end;

        {login, Return, From, Who} ->
            case orddict:find(Who, UserListA) of
                {ok, _ } ->
                    Return ! {error, "Already logged in"},
                    server(UserListA, UserListB);
                error ->
                    Return ! {ok, logged_in},
                    server(
                      orddict:store(Who, From, UserListA),
                      orddict:store(From, Who, UserListB)
                     )
            end;

        {logout, Return, From, Who} ->
            case orddict:find(Who, UserListA) of
                {ok, _ } ->
                    Return ! {ok, logged_out},
                    server(
                      orddict:erase(Who, UserListA),
                      orddict:erase(From, UserListB)
                     );
                error  ->
                    Return ! {error, "You were not logged in!"},
                    server(UserListA, UserListB)
            end

    after
        10000 ->
            io:format("Polling.....~n"),
            server(UserListA, UserListB)
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
    {server, ?SERVER} ! {logout, self(), node(), Username},

    receive
        {ok, logged_out} ->
            io:format(logged_out),
            exit(whereis(getmsg), kill);
        {error, Message} ->
            io:format(Message)
    after
        3000 ->
            io:format("Error logging out")
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
    {server, ?SERVER} ! {status, self(), node()},
    receive
        {ok, _ } ->
            {server, ?SERVER} ! {message, node(), To, Message};
        error ->
            io:format("You are not logged in!")
    end,
    ok.
