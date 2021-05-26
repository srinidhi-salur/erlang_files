-module(nth_ele).
-export([nth_ele/2]).
nth_ele([First|Rest],1)->
    First;
nth_ele([First|Rest],N) ->nth_ele(Rest,N-1).
