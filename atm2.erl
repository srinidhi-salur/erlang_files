-module(atm2).
-behavior(gen_statem).
-import(lists,[member/2]). 
-export([insert/3,stop/0,start/1,detach_card/0,insert_card/0,transaction/3,authentication/3,button/1,request/1,detach/3]).
-export([init/1,callback_mode/0,terminate/3]).
start(Pin)->
    gen_statem:start_link({local,?MODULE},?MODULE,Pin,[]).
init(Pin)->
    insert_card(),
    Data=#{pin=>Pin,length=>length(Pin),buttons => [],balance=>10000},
    {ok,insert,Data}.
insert(cast,insert_card,Data)->
       io:format("Card is inserted..~n"),  
       {next_state,authentication,Data}.
insert_card()->
     gen_statem:cast(?MODULE,insert_card).
button(Button)->
    gen_statem:cast(?MODULE,{button,Button}).
authentication(state_timeout,no_button_press,Data)->
     io:format("You took too long to press a button....~n"),
     detach_card(),
     {next_state,detach,Data};
authentication(cast,{button,Button},
          #{buttons := Buttons,length:=4,pin:=Pin}=Data)->
    %% io:format("Data:~p~n",[Data]),
    %%  io:format("Buttons:~p~n",[Buttons]),
    %%  io:format("Button:~p~n",[Button]),
        NewButtons=Buttons++[Button],
         io:format("Pressed:~p~n",[NewButtons]), 
    if NewButtons =:= Pin ->
       io:format("Valid Pin...~n"),
       {next_state,transaction,Data#{buttons:=[]},
       [{state_timeout,30000,no_request}]};
      NewButtons=/=Pin andalso length(NewButtons)==4->
           io:format("Please enter valid pin again....~n"),
           {next_state,authentication,Data#{buttons:=[]},
           [{state_timeout,30000,no_button_press}]};
     true->{next_state,authentication,Data#{buttons:=NewButtons},         
	   [{state_timeout,30000,no_button_press}]}

    end.
request(Req)->
    gen_statem:cast(?MODULE,{request,Req}).
transaction(state_timeout,no_request,Data)->
    io:format("You took long time to respond(request)....~n"),
    detach_card(),
    {next_state,detach,Data};

transaction(cast,{request,Req},Data)->
    case Req of
        {deposit,Amount}->
             Balance=maps:get(balance,Data),
             Res=Balance+Amount,
             io:format("Deposited:~p~n Balance:~p~n",[Amount,Res]),
             {next_state,transaction,Data#{balance:=Res},
             [{state_timeout,30000,no_request}]};
         {withdraw,Amount}->
             Balance=maps:get(balance,Data),
             Res=Balance-Amount,
             io:format("Withdrawn:~p~n  Balance:~p~n",[Amount,Res]),
             {next_state,transaction,Data#{balance:=Res},
             [{state_timeout,30000,no_request}]};
         {checkbalance}->
              Res=maps:get(balance,Data),
              io:format("Current balance:~p~n",[Res]),
              {next_state,transaction,Data#{balance:=Res},
              [{state_timeout,30000,no_request}]};	    
	{detach} ->
               detach_card(),
	       {next_state,detach,Data}    
     end.   
detach_card()->
    gen_statem:cast(?MODULE,detach_card).
detach(cast,detach_card,Data)->
    io:format("Card is detached..~n"),
    {next_state,insert,Data}.
callback_mode()->
    state_functions.
stop()->
    gen_statem:stop(?MODULE).
terminate(_Reason,_State,_Data)->
   void.






