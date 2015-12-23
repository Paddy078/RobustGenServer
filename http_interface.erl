-module(http_interface).
-include ("constants.hrl").
-export([start/1, stop/0]).

start(Port) ->
  stop(),
  Pid = spawn(fun () ->
    {ok, Sock} = gen_tcp:listen(Port, [{active, false}]),
    loop(Sock)
  end),
  setPid(Pid).

stop() ->
  exit(getPid(), kill).

loop(Sock) ->
  {ok, Conn} = gen_tcp:accept(Sock),
  Handler = spawn(fun () -> handle(Conn) end),
  gen_tcp:controlling_process(Conn, Handler),
  loop(Sock).

handle(Conn) ->
  Messages = chat_server:get_new_messages(),
  io:format("Messages: ~p~n", [Messages]),
  gen_tcp:send(Conn, response(Messages)),
  gen_tcp:close(Conn).

response(Str) ->
  B = iolist_to_binary(Str),
  io_lib:fwrite("HTTP/1.0 200 OK\nContent-Type: text/html\nContent-Length: ~p\n\n~s",
    [size(B), B]).


%%% Save Pid of spawned process for later to stop correctly
%%% TODO: Ugly as fuck. Change later.
setPid(Pid) ->
  put(pid, Pid).

getPid() ->
  get(pid).
