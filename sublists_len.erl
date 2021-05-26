-module(sublists_len).
-export([main/2,prefix_ls/2,sublists_len/2,zip/3]).
zip([],S,-1)->[];
zip([First|Rest],S,N)->
[{S,First,N}|zip(Rest,S+1,N-1)].
prefix_ls(_, 0) -> [];
prefix_ls([H|T],N) when N>length([H|T])->[ok];
prefix_ls([H|T], N) when N > 0 ->
    [H|prefix_ls(T, N-1)].
sublists_len([],_)->[];
sublists_len([First|Rest],Len)->
Ls1=[First|Rest],
[prefix_ls(Ls1,Len)|sublists_len(Rest,Len)].
main(Ls1,Len)->
Ls2=sublists_len(Ls1,Len),
Ls3=lists:subtract(Ls2,[[ok]]),
N=length(Ls3),
zip(Ls3,0,N-1).
