-module(lift).
-export([lift_process/0,lift_process1/3,start_lift/2]).
-compile(export_all).
start_lift(0,Max_floor)->
   io:format("Lift has from ~p--",[0]),
   io:format("~p floors..~n",[Max_floor]),
   {ok,[S]}=io:fread("Enter the calling floor:","~d"),
   %{ok,[C]}=io:fread("Enter the Current floor:","~d"),
   %{ok,[M]}=io:fread("Enter the Moving status of lift:","~d"),
   lift:lift_process1(S,0,rest).%Assume initially lift is at ground floor and at rest position
    
       
 
lift_process()->
    receive
     %{From,moving,Floors,Initial_pos,Serve_floors}->
     {From,open,Persons_in_lift,Capacity,Persons_waiting}->
        New_capacity=Capacity-Persons_in_lift,
        if New_capacity>=Persons_waiting ->
                io:format("Ok served..~n"),
                From ! {self(),ok_served};
        true->
                 io:format("Only ~p persons can be served!~nPlease close the door!! ",[New_capacity]),
		  From ! {self(),few_served}
        end,
        
        lift_process();
    
     {From,closed,D,C,M}->
       io:format("Please close the door...~n"),
       lift:lift_process1(D,C,M);
	
       
     {From,_} ->
       io:format("Cannot service!Invalid lift operation")

    end.

lift_process1(Serve_floor,Current_floor,Moving_status)when Current_floor=/=Serve_floor->
    
    case Moving_status of
         rest->io:format("Lift is at REST..~n");
         up->io:format("Lift is moving UP..~n");
         down->io:format("Lift is moving DOWN..~n");
         _->io:format("Invalid lift moving_status!~n")
    end,
     io:format("In ~p floor~n",[Current_floor]),
     if Current_floor<Serve_floor->
        Moving_status1=up,
        Current_floor1=Current_floor+1,
        io:format("Approaching floor-~p~n",[Current_floor1]),
        lift_process1(Serve_floor,Current_floor1,Moving_status1);
     Current_floor>Serve_floor->
        Moving_status1=down,
        Current_floor1=Current_floor-1,
        io:format("Approaching floor-~p~n",[Current_floor1]),
         lift_process1(Serve_floor,Current_floor1,Moving_status1);
  
     true->0
     end;      
lift_process1(Serve_floor,Current_floor,Moving_status)when Serve_floor=:=Current_floor->        
        io:format("Reached Floor-~p~n",[Current_floor]),
        Pid1=spawn(lift,lift_process,[]),
        {ok,[PL]}=io:fread("Enter No. of persons in lift:","~d"),
    {ok,[PW]}=io:fread("Enter No. of persons waiting in the called floor:","~d"),
         Pid1!{self(),open,PL,10,PW},
         if PW==0->
            io:format("No one is waiting in the floor.~n"),
            exit("No one!");
         true-> 
         {ok,[Rs]}=io:fread("Button pressed:","~d"),    
         case Rs of
          0->exit("Lift is at rest again!");
          1-> {ok,[D]}=io:fread("Enter the destination floor:","~d"),
              Pid1!{self(),closed,D,Current_floor,Moving_status}
           
        end
       end.

       
    
