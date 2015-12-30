-module(chat_server).
-include ("constants.hrl").
-include ("message.hrl").

-export([start/0, post_new_message/2, get_new_messages/0, stop/0]).

start() ->
	gen_server:start({local, ?CALLBACKS}, ?CALLBACKS, [], [{timeout, ?TIMEOUT}]),
	http_interface:start(8080).

post_new_message(From, Message_Text) ->
	io:format("~p // ~p~n~n", [From, Message_Text]),
	Msg = #{from => From, message => Message_Text},
	gen_server:call(?CALLBACKS, {post_new_message, Msg}).

get_new_messages() ->
	gen_server:call(?CALLBACKS, {get_new_messages}).

stop()->
	gen_server:cast(self(), stop).
