-module(inc_list).
-export([fst/1,rep_ele/1,main/1]).
fst([])->
    true;
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
main(Ls1)->
    Res=rep_ele(Ls1),
    Ls2=lists:subtract(Ls1,Res),
    {Res,Ls2}.
