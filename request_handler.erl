-module (request_handler).
-export ([handle_request/2]).


handle_request(#{ method := get, path := msg }, _) ->
  chat_server:get_new_messages();

handle_request(#{ method := post, path := msg, body := Body }, _) ->
  chat_server:post_new_message("me", "Hello, World!"),
  "".
