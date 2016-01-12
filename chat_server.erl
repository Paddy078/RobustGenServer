-module(chat_server).
-include ("constants.hrl").
-include ("message.hrl").

-export([start/0, post_new_message/2, post_new_async_message/2,
 					get_new_messages/1, stop/0, post_new_messages/1,
					get_registered_users/0, post_new_personal_message/3,
					get_new_personal_messages/3, post_new_personal_messages/1,
					post_new_personal_async_message/3]).

%%% Life Cycle
start() ->
	gen_server:start({local, ?CALLBACKS}, ?CALLBACKS, [], [{timeout, ?TIMEOUT}]),
	http_interface:start(8080).

stop()->
	http_interface:stop(),
	gen_server:cast(?CALLBACKS, stop).

%%% Multi Chat (Chat Room)
post_new_message(From, MessageText) ->
	io:format("~p: [FROM:~p // MESSAGE:~p] (synchron group message)~n", [current_time:get_current_time(), From, MessageText]),
	Message = #{id => -1, from => From, message => MessageText, time => current_time:get_current_time()},
	gen_server:call(?CALLBACKS, {post_new_message, Message}).

post_new_messages([]) ->
 ok;

post_new_messages(MessageList) ->
	[First|Other] = MessageList,
	#{from := From, message := Message} = First,
	post_new_async_message(From, Message),
	post_new_messages(Other).

post_new_async_message(From, MessageText) ->
	io:format("~p: [FROM:~p // MESSAGE:~p] (asynchron group message)~n", [current_time:get_current_time(), From, MessageText]),
	Message = #{id => -1, from => From, message => MessageText, time => current_time:get_current_time()},
	gen_server:cast(?CALLBACKS, {post_new_message, Message}).

get_new_messages(LastMessageIndex) ->
	gen_server:call(?CALLBACKS, {get_new_messages, LastMessageIndex}).

%%% Single chat
get_registered_users() ->
	gen_server:call(?CALLBACKS, get_registered_users).

post_new_personal_message(From, MessageText, To) ->
  io:format("~p: [FROM:~p // TO:~p // MESSAGE:~p] (synchron personal message)~n", [current_time:get_current_time(), From, To, MessageText]),
	Message = #{id => -1, from => From, to => To, message => MessageText, time => current_time:get_current_time()},
	gen_server:call(?CALLBACKS, {post_new_personal_message, Message}).

post_new_personal_messages(MessageList) ->
	case MessageList of
		[] ->
			"";
		_ ->
			[First|Other] = MessageList,
			#{from := From, message := MessageText, to := To} = First,
			post_new_personal_async_message(From, MessageText, To),
			post_new_personal_messages(Other)
	end.

post_new_personal_async_message(From, MessageText, To) ->
	io:format("~p: [FROM:~p // TO:~p // MESSAGE:~p] (asynchron personal message)~n", [current_time:get_current_time(), From, To, MessageText]),
	Message = #{id => -1, from => From, to => To, message => MessageText, time => current_time:get_current_time()},
	gen_server:cast(?CALLBACKS, {post_new_personal_message, Message}).

get_new_personal_messages(LastMessageIndex, User1, User2) ->
	gen_server:call(?CALLBACKS, {get_new_personal_messages, LastMessageIndex, User1, User2}).
