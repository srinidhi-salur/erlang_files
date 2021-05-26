-module(message).
-export([start_server/1,client/2,loop/0]).
start_server(Node) -> spawn(Node,message, loop, []).
loop() ->
receive
{From,{add, A, B}} ->
From!A+B,
loop();

{From,{mul, A, B}} ->
From!A*B,
loop();

{From,{minus, A, B}}->
    
From!A-B,
loop();

{From,{division, A, B}} ->
From!A/B,
loop();

_Other ->
loop()
end.

client(Pid,Request)->
Pid ! {self(), Request},
receive
Response ->io:format("Response is ~p~n" ,[Response])
end.
