-module(client1).
-export([client/2,start_server/0]).
 
client(Pid_server,Request) ->
    Pid_server ! {self(),Request},
    receive
        {Pid,{deposited,Amount}} ->
            io:format("New amount after depositing is ~p.~n",[Amount]),
            io:format("Successfully deposited..~n");
        {Pid,{checkedbalance,Amount}}->
            io:format("Current Balance is ~p.~n",[Amount]),
            io:format("Balance is checked..~n");
        {Pid,{withdrawn,Amount}} -> 
             io:format("New amount after withdrawl is ~p~n",[Amount]),
            io:format("Amount withdrawn successfully..~n")
    after 10000 ->
        io:format("~p~n",["No request from the user..~n"])
    end.
 
start_server()->
    spawn(server1,start1,[10000]).
    
