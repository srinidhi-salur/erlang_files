-module(arithmetic).
-export([arith/3]).
arith(addition,A,B)->
if A>0 andalso B>0 orelse A<0 andalso B<0->
	A+B;
   true ->not_possible
end;
arith(sub,A,B)->
A-B;
arith(division,A,B)->
A/B.
