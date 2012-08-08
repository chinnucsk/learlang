-module(chatapp).
-export([startServer/0,sendMessage/2,server/1,login/1,logout/1,receiver/0]).

startServer() ->
    % starts the server and registers it's handle.
    register(server, spawn(chatapp, server, [orddict:new()])),
    ok.

server(UserList) ->
    % recursive loop which constantly reads it's mailbox to find
    % instructions from clients.
    receive

        {message, From, To, Message} ->
	    case orddict:find(From, UserList) of
		{ok, _ } ->
		    case orddict:find(To, UserList) of
			{ok, _ } ->
			    io:format("Message for: ~p. With: ~p~n", [To, Message]),
			    {getmsg, orddict:fetch(To)} ! {message, Message},
			    server(UserList);
			error ->
			    {getmsg, From} ! {message, "Recipient not found!"},
			    server(UserList)
		    end;
		error ->
		    {getmsg, From} ! {message, "You are not logged in!"},
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

login(Username) ->
    % starts a client receive channel.
    register(getmsg, spawn(chatapp, receiver, [])),
    {server, server@archbox} ! {login, self(), Username},
    ok.

logout(Username) ->
    {server, server@archbox} ! {logout, self(), Username},
    ok.

receiver() ->
    % recursive loop which constantly reads it's mail box to find
    % messages to write to stdout.
    receive
        {message, Message} ->
            io:format("~p~n", [Message]),
            receiver()
    end.

sendMessage(To, Message) ->
    % send a message to the server which is intended for a specific
    % recipient.
    {server, server@archbox} ! {message, self(), To, Message},
    ok.
