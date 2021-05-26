-module(if_else).
-export([pos_neg/1]).
pos_neg(Num1)-> 
   if Num1>0->io:fwrite("positive~n");
   Num1<0->io:fwrite("Negative~n");
      true ->io:fwrite("Zero~n")
   end.
