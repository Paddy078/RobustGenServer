-module (request_handler).
-export ([handle_request/2]).

%public chat
handle_request(#{ method := get, path := {get_new_public_messages,LastMessageIndex} }, _) ->
  chat_server:get_new_public_messages(LastMessageIndex);

handle_request(#{ method := post, path := post_new_public_message, body := Body }, _) ->
  NewMessageList = json_parser:json_to_erlang(Body),
  chat_server:post_new_public_messages(NewMessageList),
  "";

%private chat
handle_request(#{ method := get, path := get_registered_users }, _) ->
  chat_server:get_registered_users();

handle_request(#{ method := get, path := {get_new_private_messages,LastMessageIndex,User1,User2} }, _) ->
    chat_server:get_new_private_messages(LastMessageIndex, User1, User2);

handle_request(#{ method := post, path := post_new_private_message, body := Body }, _) ->
  NewMessageList = json_parser:json_to_erlang(Body),
  chat_server:post_new_private_messages(NewMessageList),
  "".
