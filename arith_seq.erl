-module(arith_seq).
-export([fst/1,rep_ele/2,main/1,diff/2]).
fst([])->
   true;
fst([First|Rest]) ->
First.
diff(X,Y)->
    Y-X.
rep_ele(Diff,[])->
    true;
rep_ele(Diff,[First|Rest]) ->
Sec=fst(Rest),
    Res=diff(First,Sec),
    if Res==Diff->
	    [First|rep_ele(Diff,Rest)];
       true ->[First]
    end.
main([First|Rest])->
    Sec=fst(Rest),
    Diff=Sec-First,
    Res=rep_ele(Diff,[First|Rest]),   
    Ls2=lists:subtract([First|Rest],Res),
    {Res,Ls2}.
