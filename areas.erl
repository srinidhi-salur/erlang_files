-module(areas).
-export([start/0,shape_areas/0]).
shape_areas()->
    receive
    {From,{square,Side}}->
        Res=Side*Side,
	io:format("Area of square is:~p~n",[Res]),
	shape_areas();
    {From,{rectangle,Length,Breadth}} ->
        Res=Length*Breadth,
        io:format("Area of rectangle is:~p~n",[Res]),
	shape_areas();
    {From,{circle,Radius}}->
        Res=3.14*Radius*Radius,
	io:format("Area of circle is:~p~n",[Res]),
	shape_areas()
   % {_,_}->
    %    io:format("wrong_shape~n")	
        %wrong_shape
     end.
start()->
    spawn(areas,shape_areas,[]).
