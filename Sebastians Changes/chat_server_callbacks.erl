-module(chat_server_callbacks).
-behavior(gen_server).
-include ("constants.hrl").
-include ("message.hrl").

-export ([init/1, terminate/2, handle_call/3, handle_cast/2, handle_info/2, code_change/3]).

%%% Callbacks
%% Life Cycle
init([]) ->
	AllMessages = [],
	{ok, AllMessages}.

terminate(normal, Messages) ->
	io:format("Terminate called: normal~n", []),
	io:format("Messages at terminate: ~p~n", [Messages]),
	exit(self(), ok);
terminate(shutdown, Messages) ->
	io:format("Terminate called: shutdown~n", []),
	io:format("Messages at terminate: ~p~n", [Messages]),
	exit(self(), ok);
terminate({shutdown, Reason}, Messages) ->
	io:format("Terminate called: shutdown, ~p~n", [Reason]),
	io:format("Messages at terminate: ~p~n", [Messages]),
	exit(self(), ok);
terminate(Other, Messages) ->
	io:format("Terminate called: ~p~n", [Other]),
	io:format("Messages at terminate: ~p~n", [Messages]),
	exit(self(), ok).

%% Synchronous Calls
handle_call({post_new_message, NewMessage}, _, AllMessages) ->
	NewAllMessages = [NewMessage|AllMessages],
	{reply, ok, NewAllMessages};

handle_call({get_new_messages}, _, AllMessages) ->
	{reply, AllMessages, AllMessages}.

%% Asynchronous Calls
handle_cast(stop, Messages) ->
	io:format("Async stop request~n"),
	{stop, normal, Messages};

handle_cast({post_new_message, Msg}, AllMessages) ->
	NewAllMessages = [Msg|AllMessages],
	{noreply, NewAllMessages};

handle_cast(Request, Messages) ->
	io:format("Async request with: ~p~n", [Request]),
	{noreply, Messages}.

%% Other
handle_info(timeout, Messages) ->
	io:format("Timeout hit!~n", []),
	{noreply, Messages};
handle_info(Info, Messages) ->
	io:format("Unexpected message: ~p~n", [Info]),
	{noreply, Messages}.

code_change(_, Messages, _) ->
	% not implemented
	io:format("Code Change called!~n", []),
	{ok, Messages}.
