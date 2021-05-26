-module(inc_lists).
-export([fst/1,rep_ele/1,main/1]).
fst([])->
    0;
fst([First|Rest]) ->
First.
rep_ele([])->
    true;
rep_ele([First|Rest]) ->
   Res=fst(Rest),
   if First=<Res->
	   [First|rep_ele(Rest)];
      true ->[First]
   end.
   
main([])->
    [];
main([First|Rest]) ->
Res=rep_ele([First|Rest]),
    Ls2=lists:subtract([First|Rest],Res),
    [Res|main(Ls2)].
