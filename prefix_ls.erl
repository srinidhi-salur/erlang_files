-module(prefix_ls).
-export([prefix_ls/2]).
prefix_ls(_,0)->
    [];
prefix_ls([H|T],N) when N>length([H|T])->
    true;
prefix_ls([H|T],N)when N>0->
    [H|prefix_ls(T,N-1)].
