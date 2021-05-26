-module(split).
-export([split/2]).
split(List,Len)->
    
[lists:sublist(List,X,Len) || X<-lists:seq(1,length(List),Len)].
