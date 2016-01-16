-module(chat_server).
-include ("constants.hrl").
-include ("message.hrl").

-export([start/0, post_new_public_message/2, post_new_public_async_message/2,
 					get_new_public_messages/1, stop/0, post_new_public_messages/1,
					get_registered_users/0, post_new_private_message/3,
					get_new_private_messages/3, post_new_private_messages/1,
					post_new_private_async_message/3]).

%%% Life Cycle
start() ->
	gen_server:start({local, ?CALLBACKS}, ?CALLBACKS, [], [{timeout, ?TIMEOUT}]),
	http_interface:start(8080).

stop()->
	http_interface:stop(),
	gen_server:cast(?CALLBACKS, stop).

%%% public chat
post_new_public_message(From, MessageText) ->
	io:format("~p: [FROM:~p // MESSAGE:~p] (synchron public message)~n", [current_time:get_current_time(), From, MessageText]),
	Message = #{id => -1, from => From, message => MessageText, time => current_time:get_current_time()},
	gen_server:call(?CALLBACKS, {post_new_public_message, Message}).

post_new_public_messages([]) ->
 ok;

post_new_public_messages(MessageList) ->
	[First|Other] = MessageList,
	#{from := From, message := Message} = First,
	post_new_public_message(From, Message),
	post_new_public_messages(Other).

post_new_public_async_message(From, MessageText) ->
	io:format("~p: [FROM:~p // MESSAGE:~p] (asynchron public message)~n", [current_time:get_current_time(), From, MessageText]),
	Message = #{id => -1, from => From, message => MessageText, time => current_time:get_current_time()},
	gen_server:cast(?CALLBACKS, {post_new_public_message, Message}).

get_new_public_messages(LastMessageIndex) ->
	gen_server:call(?CALLBACKS, {get_new_public_messages, LastMessageIndex}).

%%% private chat
get_registered_users() ->
	gen_server:call(?CALLBACKS, get_registered_users).

post_new_private_message(From, MessageText, To) ->
  	io:format("~p: [FROM:~p // TO:~p // MESSAGE:~p] (synchron private message)~n", [current_time:get_current_time(), From, To, MessageText]),
	Message = #{id => -1, from => From, to => To, message => MessageText, time => current_time:get_current_time()},
	gen_server:call(?CALLBACKS, {post_new_private_message, Message}).

post_new_private_messages([]) ->
 ok;

post_new_private_messages(MessageList) ->
	[First|Other] = MessageList,
	#{from := From, message := MessageText, to := To} = First,
	post_new_private_message(From, MessageText, To),
	post_new_private_messages(Other).

post_new_private_async_message(From, MessageText, To) ->
	io:format("~p: [FROM:~p // TO:~p // MESSAGE:~p] (asynchron private message)~n", [current_time:get_current_time(), From, To, MessageText]),
	Message = #{id => -1, from => From, to => To, message => MessageText, time => current_time:get_current_time()},
	gen_server:cast(?CALLBACKS, {post_new_private_message, Message}).

get_new_private_messages(LastMessageIndex, User1, User2) ->
	gen_server:call(?CALLBACKS, {get_new_private_messages, LastMessageIndex, User1, User2}).
