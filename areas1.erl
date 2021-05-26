-module(areas1).
-export([start1/3,shape_area/0,invoke/0]).
shape_area()->
    receive
    {From,{square,Side}}->
        Res=Side*Side,
        From ! Res,
        io:format("Area of square with side=~p is:~p~n",[Side,Res]),
	shape_area();
    {From,{rectangle,Length,Breadth}} ->
        Res=Length*Breadth,
        From !Res,
        %io:format("Area of rectangle is:~p~n",[Res]),
	shape_area();
    {From,{circle,Radius}}->
        Res=3.14*Radius*Radius,
        From ! Res,
	%io:format("Area of circle with radius=~p is:~p~n",[Radius,Res]),
	shape_area(); 
    _ ->
       io:format("Other shape~n"),
	shape_area()
     end.
start1(Pid,Request,Reqnum)->
    Pid!{self(),Request},
    receive
     Result->
      io:format("Area of Request number=~p is ~p~n",[Reqnum,Result])
     after 10000-> 
      io:format("timeout~n")
    end.
invoke()->
    Pid1=spawn(areas1,shape_area,[]),
    register(shape,Pid1),
    spawn(areas1,start1,[shape,{circle,5},1]),
    spawn(areas1,start1,[shape,{rectangle,5,7},2]),  
    spawn(areas1,start1,[shape,{square,7},3]),
    spawn(areas1,start1,[shape,{test,5,7},4]),
    spawn(areas1,start1,[shape,{circle,9},5]).
 
 
    
