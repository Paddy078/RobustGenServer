-module(chat_server).
-behavior(gen_server).

-record(message, {from, message_text}).

-export([start/0, post_new_message/2, get_new_messages/0, stop/0, init/1, handle_call/3, handle_call/2, terminate/2]).

start() ->
	gen_server:start({local, ?MODULE}, ?MODULE, [], []).

post_new_message(From, Message_Text) ->

	io:format("~p // ~p", [From, Message_Text]),

	gen_server:call(?MODULE, post_new_message, #message{from=From, message_text=Message_Text}).

get_new_messages() ->
	gen_server:call(?MODULE, get_new_messages).

stop()->
	gen_server:cast(self(), stop).


%Callbacks

init([]) ->
	AllMessages = [],
	{ok, AllMessages}.

handle_call(post_new_message, NewMessage, AllMessages) ->


	NewAllMessages = [NewMessage|AllMessages],

	{reply, NewAllMessages}.

handle_call(get_new_messages, AllMessages) ->
	{reply, AllMessages}.

terminate(normal, AllMessages) ->
	%not working
	exit(self(), ok).
