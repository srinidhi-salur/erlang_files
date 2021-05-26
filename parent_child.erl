-module(parent_child).
-compile([export_all]).
child() ->
    receive
        {msg,Num} ->
            io:format(" I(child)~p received your msg~p~n", [self(),Num]),
            child()
       after 20000 ->
        io:format("Child got exited with Pid=~p~n", [self()]),
        exit(sudden_abort)
    end.
parent() ->
    Pid = spawn(parent_child, child, []),
    register(child, Pid),
    Reference = erlang:monitor(process,Pid),
    child!{msg,1},
    child!{msg,2},
    io:format("I'm parent ~p monitoring the child~n", [Pid]),
     receive
                 
	 {'DOWN', Reference, process,Pid, Reason} ->
	     io:format(" My child ~p died and the reason is(~p)~n", [Pid, Reason])       
    end,
    erlang:demonitor(Reference).
start() ->
    ParentPid = spawn(parent_child, parent, []),
    register(parent, ParentPid),
    ok.
     

