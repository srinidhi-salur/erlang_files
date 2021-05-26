-module(sublist_pos).
-export([sublist_pos/3]).
sublist_pos(Ls1,Pos,Len)->
    lists:sublist(Ls1,Pos+1,Len).
