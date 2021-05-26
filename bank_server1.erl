-module(bank_server1).
-behaviour(gen_server).
-export([start_link/0,get_balance/0,deposit/1,withdraw/1]).
-export([init/1,handle_call/3,handle_cast/2]).
-record(state,{balance::integer()}).
start_link()->
    gen_server:start_link({local,?MODULE},?MODULE,{},[]).
init({})->
    {ok,#state{balance=10000}}.
get_balance()->
    gen_server:call(?MODULE,{get_balance}).
deposit(Amount)->
    gen_server:cast(?MODULE,{deposit,Amount}).
withdraw(Amount)->
    gen_server:cast(?MODULE,{withdraw,Amount}).
handle_call({get_balance},_From,State)->
    {reply,{ok,State#state.balance},State};
handle_call(Request,_From,State)->
    error_logger:warning_msg("Bad request:~p~n",[Request]),
 
{reply,{error,unknown_call},State}.
handle_cast({deposit,Amount},#state{balance=Balance}=State)->
     Res=Balance+Amount,
     io:format("Balance amount is ~p~n",[Res]),
     {noreply,State#state{balance=Res}};
handle_cast({withdraw,Amount},#state{balance=Balance}=State)->
     if Amount=<Balance->
         Res=Balance-Amount,
         io:format("Balance amount is ~p~n",[Res]);
      true ->
         Res=Balance,
         io:format("No sufficient balance to withdraw...Available balance is ~p~n",[Balance])
      end,
      {noreply,State#state{balance=Res}};
handle_cast(Msg,State)->
     error_logger:warning_msg("Bad request: p~n",[Msg]),
     {noreply,State}.
handle_info(Info,State)->
     error_logger:warning_msg("Bad request :~p~n",[Info]),
     {noreply,State}.
terminate(_Reason,_State)->
    ok.
