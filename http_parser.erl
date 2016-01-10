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
  %PathAtom = case Path of
  PathAtom = case string:substr(Path,1,4) of
    "/msg" ->
      case length(Path) of
        4 ->
          msg;
        _ ->
          {msg, string:substr(Path,6)}
      end;
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
