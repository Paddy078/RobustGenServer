-module (json_parser).
-export ([json_to_erlang/1, erlang_to_json/1]).

%% Parses a JSON string to an Erlang Map
%% TODO: Implement actual parsing
json_to_erlang(Json) ->
  #{ from => "me", message => "Hello, World!" }.

%% Serializes an Erlang Map to a JSON String
%% TODO: Implement actual serialization
erlang_to_json(Map) when is_map(Map) ->
  "{\n\t\"from\": \"me\",\n\t\"message\": \"Hello, World!\"\n}";
erlang_to_json(List) when is_list(List) ->
  erlang_to_json(List, "[").

erlang_to_json([], Acc) ->
  Acc ++ "]";
erlang_to_json(List, Acc) ->
  io:format("~p~n", [List]),
  io:format("~p~n", [Acc]),
  [Map|Tail] = List,
  Element = erlang_to_json(Map),
  case Tail of
    [] ->
      erlang_to_json(Tail, Acc ++ Element);
    _ ->
      erlang_to_json(Tail, Acc ++ Element ++ ", ")
  end.
