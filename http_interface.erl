-module(http_interface).
-include ("constants.hrl").
-export([start/1, stop/0]).

start(Port) ->
  stop(),
  Pid = spawn(fun () ->
    {ok, Sock} = gen_tcp:listen(Port, [{active, false}]),
    loop(Sock)
  end),
  setPid(Pid),
  ok.

stop() ->
  case getPid() of
    undefined ->
      ok;
    Pid ->
      exit(Pid, kill)
  end.


loop(Sock) ->
  {ok, Conn} = gen_tcp:accept(Sock),
  Handler = spawn(fun () -> handle(Conn) end),
  gen_tcp:controlling_process(Conn, Handler),
  loop(Sock).

handle(Socket) ->
  {ok, Request} = gen_tcp:recv(Socket, 0),
  ParsedRequest = http_parser:parse(Request),
  Answer = request_handler:handle_request(ParsedRequest, Socket),
  SerializedAnswer = json_parser:erlang_to_json(Answer),
  gen_tcp:send(Socket, response(SerializedAnswer)),
  gen_tcp:close(Socket).

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
