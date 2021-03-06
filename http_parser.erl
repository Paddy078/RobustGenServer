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
  PathAtom = case string:substr(Path,1,15) of
    "/public_message" ->
      case length(Path) of
        15 -> %url = .../public_message
          post_new_public_message;
        _ ->
	  {LastMessageIndex,_} = string:to_integer(string:substr(Path,17)),
          {get_new_public_messages, LastMessageIndex} %second element in tuple is index of last message the client already has. to genereate message delta
      end;
    "/private_messag" -> %url = .../personalMsg...
      case length(Path) of
        16 -> %url = .../private_message
          post_new_private_message;
        _ -> %url z.B. = .../private_message/Patrick/Timo/4  ===== messages between Patrick to Timo
          StringToSplit = string:substr(Path, 18),
          SplittedString = string:tokens(StringToSplit, "/"),
          User1 = lists:nth(1, SplittedString),
          User2 = lists:nth(2, SplittedString),
	  {LastMessageIndex,_} = string:to_integer(lists:nth(3, SplittedString)),
          {get_new_private_messages, LastMessageIndex, User1, User2}
        end;
      "/get_users" ->
       		get_registered_users;
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
