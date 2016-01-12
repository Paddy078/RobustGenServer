-module (current_time).
-export ([get_current_time/0]).

get_current_time() ->
	{{_,_,_},{Hour,Minutes,Seconds}} = erlang:localtime(),
	FormattedTime = io_lib:fwrite("~2.2.0w:~2.2.0w:~2.2.0w",[Hour,Minutes,Seconds]),
	lists:flatten(FormattedTime).
