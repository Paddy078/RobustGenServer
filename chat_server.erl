-module(chat_server).
-include ("constants.hrl").
-include ("message.hrl").

-export([start/0, post_new_message/2, post_new_async_message/2, get_new_messages/1, stop/0, post_new_messages/1]).

start() ->
	gen_server:start({local, ?CALLBACKS}, ?CALLBACKS, [], [{timeout, ?TIMEOUT}]),
	http_interface:start(8080).

post_new_message(From, Message_Text) ->
	io:format("new sync message: ~p // ~p~n~n", [From, Message_Text]),

	Msg = #{id => -1, from => From, message => Message_Text, time => get_formatted_current_time()},
	gen_server:call(?CALLBACKS, {post_new_message, Msg}).

post_new_messages([]) ->
 ok;

post_new_messages(MessageList) ->
	[First|Other] = MessageList,
	#{from := From, message := Message} = First,
	post_new_async_message(From, Message),
	post_new_messages(Other).

post_new_async_message(From, Message_Text) ->
	io:format("new async message: ~p // ~p ~n~n", [From, Message_Text]),
	Msg = #{id => -1, from => From, message => Message_Text, time => get_formatted_current_time()},
	gen_server:cast(?CALLBACKS, {post_new_message, Msg}).

get_new_messages(LastMessageIndex) ->
	gen_server:call(?CALLBACKS, {get_new_messages, LastMessageIndex}).

stop()->
	http_interface:stop(),
	gen_server:cast(?CALLBACKS, stop).

get_formatted_current_time() ->
	{Date={Year,Month,Day},Time={Hour,Minutes,Seconds}} = erlang:localtime(),
	FormattedTime = io_lib:fwrite("~2.2.0w:~2.2.0w:~2.2.0w~n",[Hour,Minutes,Seconds]),
	lists:flatten(FormattedTime).
