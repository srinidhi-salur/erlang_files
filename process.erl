-module(process).
-export([spawns/1]).
-compile(export_all).

spawns(X)->
   G=fun(X) -> io:format("~p~n",[X*2]) end,
   spawn(fun()->G(X) end),

   F=fun(X)->X*3 end,
   [spawn(fun()->F(X) end)||X<-lists:seq(1,10)]. 
dolphin1() ->
receive
   do_a_flip ->io:format("How about no?~n");
   fish ->io:format("So long and thanks for all the fish!~n");
   _ ->io:format("Heh, we're smarter than you humans.~n")
end.
dolphin2() ->
receive
   {From, do_a_flip} ->From ! "How about no?";
   {From, fish} ->From ! "So long and thanks for all the fish!";
   _ ->io:format("Heh, we're smarter than you humans.~n")
end.
dolphin3() ->
receive
  {From, do_a_flip} ->From ! "How about no?",
   dolphin3(); 
  {From, fish} ->From ! "So long and thanks for all the fish!";
  _ ->io:format("Heh, we're smarter than you humans.~n"),
  dolphin3()
end.


