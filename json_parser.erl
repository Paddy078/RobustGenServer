-module (json_parser).
-export ([json_to_erlang/1, erlang_to_json/1]).

%% Parses a JSON string to an Erlang Map
%% TODO: Implement actual parsing
json_to_erlang(Json) ->
  ListElements = string:tokens(Json, "}"),
  json_to_erlang(ListElements, []).

json_to_erlang([], ElementAcc) ->
  ElementAcc;

json_to_erlang([Head|Tail], ElementAcc) ->
  ActualElement = json_element_to_erlang(Head),
  case ActualElement of
    [] ->
      json_to_erlang(Tail, ElementAcc);
    _ ->
      NewElementAcc = [ActualElement|ElementAcc],
      json_to_erlang(Tail, NewElementAcc)
  end.


json_element_to_erlang(Element) ->
  %%really ugly string editing
  %%first get rid of trailing Semikolon
  StrippedElem = string:strip(Element, left, $,),
  %%Remove some stuff
  CleanedElement = [X || X <- StrippedElem, X =/= ${, X =/= $}, X =/= $[, X =/= $], X=/=$\\, X=/=$"],
  %%Remove all \n ´s
  CleanedElement2 = re:replace(CleanedElement, "\\s+", "", [global,{return,list}]),
  %%Create a list of map keys and values
  CElementList = string:tokens(CleanedElement2, ",: "),
  case CElementList of
     ["from", From, "to", To, "message", Message]  ->
       #{from => From, to => To, message => Message};
     ["from", From, "message", Message]  ->
       #{from => From, message => Message};
     [] ->
       [];
      _ ->
        #{from => "System", to => "System", message => "Incorrect Message Format"}
  end.

%% Serializes an Erlang Map to a JSON String
%% TODO: Implement actual serialization
% The input is a map and should be serialized to a JSON object
erlang_to_json(Map) when is_map(Map) ->
  case Map of
    #{name := Name} -> %serialization of list of registered users
      #{name := Name} = Map,
      io_lib:fwrite("{\n\t\"name\": \"~p\"}", [Name]);
    _ -> %serialization of message
      #{id := Id, from := From, message := Message, time := Time} = Map,
      io_lib:fwrite("{\n\t\"id\": \"~p\", \n\t\"from\": \"~p\",\n\t\"message\": \"~p\"\n,\n\t\"time\": \"~p\"\n}", [Id, From, Message, Time])
  end;

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
