-module(chat_server).
-include ("constants.hrl").
-include ("message.hrl").

-export([start/0, post_new_message/2, post_new_async_message/2, get_new_messages/0, stop/0, post_new_messages/1]).

start() ->
	gen_server:start({local, ?CALLBACKS}, ?CALLBACKS, [], [{timeout, ?TIMEOUT}]),
	http_interface:start(8080).

post_new_message(From, Message_Text) ->
	io:format("~p // ~p~n~n", [From, Message_Text]),
	Msg = #{from => From, message => Message_Text},
	gen_server:call(?CALLBACKS, {post_new_message, Msg}).

post_new_messages([]) ->
 ok;

post_new_messages(MessageList) ->
	[First|Other] = MessageList,
	#{from := From, message := Message} = First,
	post_new_async_message(From, Message),
	post_new_messages(Other).

post_new_async_message(From, Message_Text) ->
	io:format("~p // ~p~n~n", [From, Message_Text]),
	Msg = #{from => From, message => Message_Text},
	gen_server:cast(?CALLBACKS, {post_new_message, Msg}).

get_new_messages() ->
	gen_server:call(?CALLBACKS, {get_new_messages}).

stop()->
	http_interface:stop(),
	gen_server:cast(?CALLBACKS, stop).
