-module(arith_case).
-export([arith/3]).
arith(Operation,A,B)->
case Operation of 
addition->	
if A>0 andalso B>0 orelse A<0 andalso B<0->
	A+B;
   true ->not_possible
end;
subtract->if A>0 andalso B>0 orelse A<0 andalso B<0->
	A-B;
   true ->not_possible
end;
 division->
	if A>0 andalso B>0 orelse A<0 andalso B<0->
	A/B;
	   true ->ok
end.
