-module(suffix_ls).
-export([suffix_ls/2,prefix_ls/2]).
prefix_ls(_,0)->
    [];
prefix_ls([H|T],N) when N>0 ->
    [H|prefix_ls(T,N-1)].
suffix_ls(Ls1,N)->
    Res=prefix_ls(Ls1,N),
    lists:subtract(Ls1,Res).
