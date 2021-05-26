-module(info_handler).
-behaviour(gen_event).
-export([init/1,handle_event/2,handle_call/2,handle_info/2,code_change/3,terminate/2]).
init(_Args)->
     {ok,[]}.
handle_event({info_msg,Info_msg},State)->
     io:format("****Here is your info ~s*****~n",[Info_msg]),
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



