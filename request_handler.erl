-module (request_handler).
-export ([handle_request/2]).


handle_request(#{ method := get, path := {getMsg,LastMessageIndex} }, _) ->
  chat_server:get_new_messages(LastMessageIndex);

handle_request(#{ method := post, path := postMsg, body := Body }, _) ->
  %chat_server:post_new_message("me", "Hello, World!"),
  NewMessageList = json_parser:json_to_erlang(Body),
  chat_server:post_new_messages(NewMessageList),
  "";

%single chat START%
handle_request(#{ method := get, path := getRegisteredUsers }, _) ->
  chat_server:get_registered_users();

handle_request(#{ method := get, path := {getPersonalMsg,LastMessageIndex,User1,User2} }, _) ->
    chat_server:get_new_personal_messages(LastMessageIndex, User1, User2);

handle_request(#{ method := post, path := postPersonalMsg, body := Body }, _) ->
  NewMessageList = json_parser:json_to_erlang(Body),



  chat_server:post_new_personal_messages(NewMessageList),
  "".
%single chat END%
