-module (request_handler).
-export ([handle_request/2]).


handle_request(#{ method := get, path := {msg,LastMessageIndex} }, _) ->
  chat_server:get_new_messages(LastMessageIndex);

handle_request(#{ method := post, path := msg, body := Body }, _) ->
  %chat_server:post_new_message("me", "Hello, World!"),
  NewMessageList = json_parser:json_to_erlang(Body),
  chat_server:post_new_messages(NewMessageList),
  "".
