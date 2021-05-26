-module(client).
-compile(export_all).
start()->
    bank_server:start(client).
requests()->
    io:format("Depositing..~n"),
    bank_server:call(client,{deposit,3000}),
    io:format("Current Balance Check..~n"),
    bank_server:call(client,{checkbalance}),
    io:format("Withdrawing..~n"),
    bank_server:call(client,{withdraw,5000}),
    io:format("Check Balance Check..~n"),
    bank_server:call(client,{checkbalance}).
init()->
    Balance = 10000,
    Balance.
handle_call(Request,State) ->
    case Request of 
        {deposit,D} ->
            Amount = State + D,
            Amount;
        {withdraw,W} ->
            Amount = State - W,
            Amount;
        {checkbalance} ->
            State;
        _Other ->
            wrong_request
    end.
