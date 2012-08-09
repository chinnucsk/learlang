-module(chatapp).
-author("Aaron France").
-export([startServer/0,sendMessage/2,send/2,server/2,login/1,logout/1,receiver/0,newServer/0]).
-define(SERVER, server@FRANCEA1).

newServer() ->
    spawn(?MODULE, startServer, []).

%% starts the server and registers it's Pid to server.
startServer() ->
    process_flag(trap_exit, true),
    register(server, spawn_link(?MODULE, server, [orddict:new(),orddict:new()])),
    receive
        {'EXIT', Pid, _ } ->
            io:format("Restarting process: ~p~n", [Pid]),
            startServer()
    end.

%% recursive loop which constantly reads it's mailbox to find
%% instructions from clients.
%%
%% We can deal with handling messages to clients and logging
%% in and out.
server(UserListA, UserListB) ->

    receive
        die ->
            1/0;
        {status, Return, From} ->
            Return ! orddict:find(From, UserListB),
            server(UserListA, UserListB);

        {message, Return, To, Message} ->
            case orddict:find(To, UserListA) of
                {ok, _ } ->
                    {getmsg, orddict:fetch(To, UserListA)} ! {message, Message},
                    Return ! ok,
                    server(UserListA, UserListB);
                error ->
                    Return ! error,
                    server(UserListA, UserListB)
            end;
        
        %% When we receive the 'login' atom we login the user by adding their
        %% node value as a key along with their username that they suppled
        %% as well as the reverse so we can do reverse lookups.
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
            io:format("~p....~n", [polling]),
            server(UserListA, UserListB)
    end.

%% Logs a user onto the server.
login(Username) ->
    % if we try to log in twice, kill our message queue.
    {getmsg, node()} ! exit,
    % starts a client receive channel.
    {server, ?SERVER} ! {login, self(), node(), Username},
    receive
        {ok, logged_in} ->
            io:format("~p~n", [logged_in]),
            register(getmsg, spawn(chatapp, receiver, []));
        {error, Message} ->
            Message
    after
        3000 ->
            error_logging_in
    end,
    ok.

%% Logs a user out on the server.
logout(Username) ->
    {server, ?SERVER} ! {logout, self(), node(), Username},
    
    receive
        {ok, logged_out} ->
            io:format("~p~n", [logged_out]),
            exit(whereis(getmsg), kill);
        {error, Message} ->
            io:format("~p~n", [Message])
    after
        3000 ->
            error_logging_out
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
            receiver();
        exit ->
            exit(self(), kill)
    end.

%% Checks if the user is logged in and, if so, sends the message to
%% their recipient.
send(To, Message) ->
    % send a message to the server which is intended for a specific
    % recipient.
    {server, ?SERVER} ! {status, self(), node()},
    receive
        {ok, _ } ->
            sendMessage(To, Message);
        error ->
            not_logged_in
    after
        3000 ->
            server_error
    end.

sendMessage(To, Message) ->
    {server, ?SERVER} ! {message, self(), To, Message},
    io:format("~p~n", [self()]),
    receive
        ok ->
            ok;
        error ->
            recipient_not_available
    after
        3000 ->
            server_error
    end,
    ok.
