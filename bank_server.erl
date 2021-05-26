-module(bank_server).
-compile(export_all).
start(Client)->
    spawn(bank_server,init,[Client]).
init(Client)->
    register(Client,self()),
    State=Client:init(),
    loop(Client,State).
call(Client,Request)->
    Client!{call,self(),Request},
     receive
      {Pid,Amount}->
          io:format("Current Amount is ~p.~n",[Amount])
       after 10000->
          io:format("~p~n",["No request from the user..~n"])
     end.
loop(Client,State)->
    receive
      {call,From,Request}->
          Amount=Client:handle_call(Request,State),
          From!{self(),Amount},
          loop(Client,Amount)
    end.
