-module(eventhandler).
-behaviour(gen_event).
-export([init/1,handle_event/2,handle_call/2,handle_info/2,code_change/3,terminate/2]).

init([])->
   {ok,[]}.
handle_event({all_messages,Name},State)->
    io:format("Hey hi ~s....~n",[Name]),
    io:format("How are you?~n"),
    {ok,State};
handle_event({error_msge},State)->
    io:format("******ERROR MESSAGE******~n"),
    {ok,State};
handle_event({info_msge},State)->
    io:format("///////INFO MESSAGE////////~n"),
    {ok,State}.
handle_call(_, State) ->
{ok, State}.
handle_info(_, State) ->
{ok, State}.
code_change(_OldVsn,State,_Extra)->
{ok,State}.
terminate(_Reason,_State)->
ok.
