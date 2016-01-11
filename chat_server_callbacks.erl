-module(chat_server_callbacks).
-behavior(gen_server).
-include ("constants.hrl").
-include ("message.hrl").

-export ([init/1, terminate/2, handle_call/3, handle_cast/2, handle_info/2, code_change/3]).

%%% Callbacks
%% Life Cycle
init([]) ->
	AllMessages = [],

	%single chat START%
	RegisteredUsers = ["Xaver", "Patrick", "Timo", "Sebastian"],
	PersonalMessages = [],
	%single chat END%

	{ok, {AllMessages, PersonalMessages, RegisteredUsers}}.

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
handle_call({post_new_message, NewMessage}, _, {AllMessages, PersonalMessages,RegisteredUsers}) ->
		NewMessageWithUpdatedId = get_new_message_with_updated_id(NewMessage, AllMessages),
		NewAllMessages = lists:concat([AllMessages, [NewMessageWithUpdatedId]]),
		{reply, ok, {NewAllMessages, PersonalMessages,RegisteredUsers}};

handle_call({get_new_messages, LastMessageIndex}, _, {AllMessages, PersonalMessages,RegisteredUsers}) ->
	{LastMessageIndexInt,_} = string:to_integer(LastMessageIndex),
 	MessagesToSend = [X || X <- AllMessages, maps:get(id,X) > LastMessageIndexInt], %filter messages, only get messages with id higher that lastMessageIndex
	{reply, MessagesToSend, {AllMessages, PersonalMessages,RegisteredUsers}};

%single chat START%
handle_call(get_registered_users, _, {AllMessages, PersonalMessages,RegisteredUsers}) ->
	{reply, RegisteredUsers, {AllMessages, PersonalMessages,RegisteredUsers}};

handle_call({post_new_personal_message, NewMessage}, _, {AllMessages, PersonalMessages,RegisteredUsers}) ->
	NewMessageWithUpdatedId = get_new_message_with_updated_id(NewMessage, PersonalMessages),
	NewPersonalMessages = lists:concat([PersonalMessages, [NewMessageWithUpdatedId]]),
	{reply, ok, {AllMessages, NewPersonalMessages,RegisteredUsers}};

handle_call({get_new_personal_messages, LastMessageIndex, User}, _, {AllMessages, PersonalMessages,RegisteredUsers}) ->
	{LastMessageIndexInt,_} = string:to_integer(LastMessageIndex),
	MessagesToSend = [X || X <- PersonalMessages, maps:get(id,X) > LastMessageIndexInt, (maps:get(from, X) == User) or (maps:get(to, X) == User)],
	{reply, MessagesToSend, {AllMessages, PersonalMessages,RegisteredUsers}}.

handle_cast({post_new_personal_message, NewMessage}, {AllMessages, PersonalMessages,RegisteredUsers}) ->
	NewMessageWithUpdatedId = get_new_message_with_updated_id(NewMessage, PersonalMessages),
	NewPersonalMessages = lists:concat([PersonalMessages, [NewMessageWithUpdatedId]]),
	{noreply, {AllMessages, NewPersonalMessages,RegisteredUsers}};
%single chat END%




%% Asynchronous Calls
handle_cast(stop, {AllMessages, PersonalMessages,RegisteredUsers}) ->
	io:format("Async stop request~n"),
	{stop, normal, {AllMessages, PersonalMessages,RegisteredUsers}};

handle_cast({post_new_message, Msg}, {AllMessages, PersonalMessages,RegisteredUsers}) ->
	NewMessageWithUpdatedId = get_new_message_with_updated_id(Msg, AllMessages),
	NewAllMessages = lists:concat([AllMessages, [NewMessageWithUpdatedId]]),
	{noreply, {NewAllMessages, PersonalMessages,RegisteredUsers}};

handle_cast(Request, {AllMessages, PersonalMessages,RegisteredUsers}) ->
	io:format("Async request with: ~p~n", [Request]),
	{noreply, {AllMessages, PersonalMessages,RegisteredUsers}}.

%% Other
handle_info(timeout, {AllMessages, PersonalMessages,RegisteredUsers}) ->
	io:format("Timeout hit!~n", []),
	{noreply, {AllMessages, PersonalMessages,RegisteredUsers}};
handle_info(Info, {AllMessages, PersonalMessages,RegisteredUsers}) ->
	io:format("Unexpected message: ~p~n", [Info]),
	{noreply, {AllMessages, PersonalMessages,RegisteredUsers}}.

code_change(_, {AllMessages, PersonalMessages,RegisteredUsers}, _) ->
	% not implemented
	io:format("Code Change called!~n", []),
	{ok, {AllMessages, PersonalMessages,RegisteredUsers}}.

%Helper method to update the id of a new message to the next free id
	get_new_message_with_updated_id(NewMessage, AllMessages) ->

		case length(AllMessages) of
			0 ->
				LastMessageID = 0;
			_ ->
				LastMessage = lists:last(AllMessages),
				#{id := LastMessageID} = LastMessage
		end,
		NewMessageID = LastMessageID + 1,
		NewMessageWithUpdatedId = maps:put(id, NewMessageID, NewMessage),
		NewMessageWithUpdatedId.
