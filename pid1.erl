-module(pid1).
-export([loop/0]).
%Pid=spawn(pid1,loop,[]).
%exit(Pid,kill).
loop() ->
    receive
        who_are_you ->
            io:format("I am ~p~n", [self()]),
            loop()
    end.
