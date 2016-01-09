-module(http_interface).
-include ("constants.hrl").
-export([start/1, stop/0]).

%%% TCP Server Life Cycle
%% The start/1 function spawns a new process that listens for TCP messages
start(Port) ->
  stop(),
  Pid = spawn(fun () ->
    {ok, Sock} = gen_tcp:listen(Port, [{active, false}]),
    loop(Sock)
  end),
  setPid(Pid),
  ok.

%% The stop/0 function kills the process that was spawned in start/1
stop() ->
  case getPid() of
    undefined ->
      ok;
    Pid ->
      exit(Pid, kill)
  end.

%%% Request handling
%% Listen for connections and spawn a handler if a connection occurs
loop(Sock) ->
  {ok, Conn} = gen_tcp:accept(Sock),
  Handler = spawn(fun () -> handle(Conn) end),
  gen_tcp:controlling_process(Conn, Handler),
  loop(Sock).

%% Handle Requests (so far only synchronously)
handle(Socket) ->
  {ok, Request} = gen_tcp:recv(Socket, 0),
  % The requests are parsed according to the http_parser module
  ParsedRequest = http_parser:parse(Request),
  % After parsing, the server reacts to the request as defined in
  % the request handler module
  Answer = request_handler:handle_request(ParsedRequest, Socket),
  % The answer is then serialized to JSON (if applicable) and sent back
  SerializedAnswer = json_parser:erlang_to_json(Answer),
  gen_tcp:send(Socket, response(SerializedAnswer)),
  gen_tcp:close(Socket).

%% Serialize the response to HTTP compatible format
response(Str) ->
  B = iolist_to_binary(Str),
  io_lib:fwrite("HTTP/1.0 200 OK\nContent-Type: text/html\nContent-Length: ~p\n\n~s",
    [size(B), B]).


%%% Save Pid of spawned process for later to stop correctly
%%% TODO: Ugly as fuck. Change later.
%%% TODO: Also make sure this actually works.
setPid(Pid) ->
  put(pid, Pid).

getPid() ->
  get(pid).
