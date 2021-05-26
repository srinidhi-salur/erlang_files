-module(arithmetic_op).
-export([arith_process/0]).
-compile(export_all).
arith_process()->
    receive
     {From,add,A,B}->
       io:format("Addition of A,B results in ~p~n",[A+B]),
       From ! {self(),ok_added},
       arith_process();
     {From,subtract,A,B} ->
       io:format("Subtraction of A,B results in ~p~n",[A-B]),
       From ! {self(),[A-B]},
       arith_process();
     {From,multiply,A,B} ->
       io:format("Multiplication of A,B results in ~p~n",[A*B]),
       From ! {self(),ok_multiplied},
       arith_process();
    {From,divide,A,B} ->
       io:format("Division of A,B results in ~p~n",[A/B]),
       From ! {self(),ok_divided},
       arith_process();
    {From,_,A,B}->
       io:format("Please enter valid operation!")

    end.
