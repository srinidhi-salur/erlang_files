-module(anonymous).
-export([start/0,start1/1,start2/1]).
-import(lists,[map/2]).
start()->
    F=fun(X)->
	       X+1 end,
    F(5).
%For single variable
start1(X)->
    F=fun(X)-> X*2 end,
    io:fwrite("Double of ~w is ",[X]),
    io:fwrite("~w~n",[F(X)]),
    F1=fun Fact(1)->1 ;Fact(X)when X>1->X*Fact(X-1)end,
    io:fwrite("Factorial of ~w is ",[X]),
    io:fwrite("~w~n",[F1(X)]),
    F2 = fun (X) when X>=10 -> greater_than10; (X) -> lesser_than10 end,
    F2(X).
%For entire list(in maps)
start2(L)->
	io:fwrite("~p~n",[map(fun(X)->2*X end,L)]),
        %io:fwrite("~p~n",[map(fun double/1,L)]),
        %double(X)->2*X,
        Triple=fun(X)->X*3 end,
        io:fwrite("~p~n",[map(Triple,L)]),
        if is_function(Triple)->
		io:fwrite("Triple is a fun!~n");
	   true ->io:fwrite("Triple is not a fun..")end,
       Big =  fun(X) -> if X > 10 -> true; true -> false end end,
       lists:any(Big,L).

    
