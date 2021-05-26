-module(message_handler).
-behaviour(gen_event).
-export([init/1,handle_event/2,handle_call/2,handle_info/2,code_change/3,terminate/2]).
init(_Args)->
     {ok,[]}.
handle_event({all_msg,All_msg},State)->
     io:format("Hello ~s~n",[All_msg]),
     io:format("How are you?~p~n",[All_msg]),
     {ok,State};
handle_event(_,State)->
    {ok,State}.
handle_call(_, State) ->
{ok, State}.
handle_info(_, State) ->
{ok, State}.
code_change(_OldVsn,State,_Extra)->
{ok,State}.
terminate(_Reason,_State)->
ok.
