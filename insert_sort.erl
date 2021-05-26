-module(insert_sort).
-import(lists,[sort/1]).
-export([start/1,insert_sort/2]).
start(Lst1)->
    io:fwrite("~p~n",[sort(Lst1)]).
insert_sort(Ls,Ele)->
    {Left,Right}=lists:split(length(Ls)-1,Ls),
    start(Left++[Ele|Right]).
