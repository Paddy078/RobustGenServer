-module (http_parser).
-export ([parse/1]).

%% Parses an HTTP request
parse(Request) ->
  Lines = string:tokens(Request, "\r\n"),
  [HeaderLine | _] = Lines,
  Header = read_header(HeaderLine),
  Header#{
    body => extract_body(Request)
  }.

%% Reads a header line,
%% Returns a map containing the method and the path as atoms
read_header(HeaderLine) ->
  Words = string:tokens(HeaderLine, " "),
  [Method | Rest] = Words,
  [Path | _] = Rest,
  MethodAtom = case Method of
    "POST" ->
      post;
    "GET" ->
      get;
    _ ->
      unsupported_method
  end,
  PathAtom = case Path of
    "/msg" ->
      msg;
    _ ->
      unsupported_path
  end,
  #{
    method => MethodAtom,
    path => PathAtom
  }.

%% Extracts the body of an HTTP request
%% Returns the String that represents the body or an empty list
extract_body(Request) ->
  BodySplit = string:str(Request, "\r\n\r\n"),
  Body = string:substr(Request, BodySplit + 4),
  Body.
