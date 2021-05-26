-module(tail).
-export([tails/1,main/1]).
tails([])->
    [];
tails([First|Rest]) ->
[Rest|tails(Rest)].
main([First|Rest])->
    [[First|Rest]|tails([First|Rest])].
