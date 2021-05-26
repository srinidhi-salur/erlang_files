%link(spawn(linkamon,chain,[3]))
%spawn_link(linkamon,chain,[4])
%Monitors::>>Ref=erlang:monitor(process,spawn(fun()->timer:sleep(500) end)).
%erlang:demonitor(Ref).
%spawn_monitor(fun()->receive _->exit(boom) end end)

-module(linkamon).
-compile(export_all).
myproc() ->
   timer:sleep(5000),
   exit(reason).

chain(0) ->
   receive
     _ -> ok
     after 2000 ->exit("chain dies here")
   end;
chain(N) ->
   Pid = spawn(fun() -> chain(N-1) end),
   link(Pid),
   receive
     _-> ok
   end.
