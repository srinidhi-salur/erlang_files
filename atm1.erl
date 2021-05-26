-module(atm1).
-behavior(gen_statem).
-import(lists,[member/2]). 
-export([insert/3,stop/0,start/0,detach_card/0,insert_card/0,transaction/3,authentication/3,button/1,request/1,detach/3]).
-export([init/1,callback_mode/0,terminate/3]).
start()->
    gen_statem:start_link({local,?MODULE},?MODULE,{},[]).
init({})->
    insert_card(),
    Data=#{pins=>[[1,2,3,4],[4,5,6,7]],buttons => [],balance=>10000},
    {ok,insert,Data}.

insert(cast,insert_card,Data)->
       io:format("Your card is inserted..~n"),  
       {next_state,authentication,Data}.
insert_card()->
     gen_statem:cast(?MODULE,insert_card).
button(Button)->
    gen_statem:cast(?MODULE,{button,Button}).
authentication(cast,{button,Button},
          #{buttons := Buttons}=Data)->
    io:format("Data:~p~n",[Data]),
       NewButtons=
        if length(Buttons)<4 ->
          io:format("Buttons:~p~n",[Buttons]),
          Buttons;
        true->
           io:format("Enter the valid pin again....~n"),
           {next_state,authentication,Data#{buttons:=[]}}
        end++[Button],
    io:format("New Buttons :~p~n",NewButtons),
    P=maps:get(pins,Data),
    L=member(NewButtons,P),
    if L->
       io:format("Valid Pin...~n"),
       {next_state,transaction,Data#{buttons:=[]}};
       true->
          {next_state,authentication,Data#{buttons :=NewButtons}}      
    end.
request(Req)->
    gen_statem:cast(?MODULE,{request,Req}).
transaction(cast,{request,Req},Data)->
    case Req of
        {deposit,Amount}->
             Balance=maps:get(balance,Data),
             Res=Balance+Amount,
             io:format("Amount is after depositing : ~p~n",[Res]),
             {next_state,transaction,Data#{balance:=Res}};
         {withdraw,Amount}->
             Balance=maps:get(balance,Data),
             Res=Balance-Amount,
             io:format("After withdrawal amount is :~p ~n",[Res]),
             {next_state,transaction,Data#{balance:=Res}};
         {checkbalance}->
              Res=maps:get(balance,Data),
              io:format("Balance is :~p ~n",[Res]),
              {next_state,transaction,Data#{balance:=Res}};	    
	{detach} ->
               detach_card(),
	       {next_state,detach,Data}
	     
     end.
   
detach_card()->
    gen_statem:cast(?MODULE,detach_card).
detach(cast,detach_card,Data)->
    io:format("Your card is detached..~n"),
    {next_state,insert,Data}.
callback_mode()->
    state_functions.
stop()->
    gen_statem:stop(?MODULE).
terminate(_Reason,_State,_Data)->
   void.

