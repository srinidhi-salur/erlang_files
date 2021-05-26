-module(insert_nth).
-export([insert_nth/3]).
insert_nth(Ls,Ele,Pos)->
    {Left,Right}=lists:split(Pos-1,Ls),
    Left++[Ele|Right].
