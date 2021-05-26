-module(repeat).
-export([fst/1,rep_ele/1]).
fst([])->
    true;
fst([First|Rest]) ->
    First.
rep_ele([])->
    true;
rep_ele([First|Rest]) ->
    Res=fst(Rest),
    if First==Res->
	    [First|rep_ele(Rest)];
    true ->rep_ele(Rest)
    end.
