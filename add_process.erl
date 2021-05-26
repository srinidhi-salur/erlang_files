-module(add_process).
-export([add_process/2]).
-compile(export_all).
add_process(A,B)->
    receive
     {From,add}->
     %io:format("~p~n",[A+B]),
     From ! {self(),[A+B]}
    end.
