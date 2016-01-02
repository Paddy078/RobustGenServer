-module (json_parser).
-export ([json_to_erlang/1, erlang_to_json/1]).

%% Parses a JSON string to an Erlang Map
%% TODO: Implement actual parsing
json_to_erlang(Json) ->
  #{ from => "me", message => "Hello, World!" }.

%% Serializes an Erlang Map to a JSON String
%% TODO: Implement actual serialization
% The input is a map and should be serialized to a JSON object
erlang_to_json(Map) when is_map(Map) ->
  "{\n\t\"from\": \"me\",\n\t\"message\": \"Hello, World!\"\n}";
% The input is an array and should be serialized to a JSON array
erlang_to_json(List) when is_list(List) ->
  erlang_to_json(List, "[");
% The Input isn't serializable to JSON and should be returned unchanged
erlang_to_json(Some) ->
  Some.

%% Private implementation of Erlang list to json serialization
erlang_to_json([], Acc) ->
  Acc ++ "]";
erlang_to_json(List, Acc) ->
  [Map|Tail] = List,
  Element = erlang_to_json(Map),
  case Tail of
    [] ->
      erlang_to_json(Tail, Acc ++ Element);
    _ ->
      erlang_to_json(Tail, Acc ++ Element ++ ", ")
  end.
