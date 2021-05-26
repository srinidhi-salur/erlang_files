
-module(lift).
-behaviour(gen_server).
%%API Functaions
-export([start/1,stop/1,info/1]).

%%Callback Functions
-export([init/1,handle_call/3,handle_cast/2,handle_continue/2,handle_info/2,terminate/2]).

-define(LC,{global,liftcontroller}).

start({Name,Ground,Top,Cap,Speed}) ->
    case gen_server:start({local,Name},?MODULE,
			  {Ground,Top,Cap,Speed},[]) of
	{ok,PID} ->
	    %%Registering lift process
	    gen_event:sync_notify(?LC,{newlift,{Name,PID}});
	{already_started,PID} ->
	    io:format("Lift already registered ~p~n",[PID]);
	Error->
	    io:format("Error ~p ~n",[Error])
    end.

stop(Name) ->
    gen_server:cast(Name,{stop,Name}).
info(Name) ->
    gen_server:cast(Name,{info}).

init({Ground,Top,Cap,Speed}) ->
    put(ground,Ground),
    put(speed,Speed),
    put(top,Top),
    put(cap,Cap),
    put(current_position,0),
    put(ustops,[]),
    put(dstops,[]),
    put(moving_direction,idle),
    {ok,[]}.
handle_call({stop,Name},_From,_State) ->
    io:format("Stoping lift"),
    gen_event:sync_notify(?LC,{removelift,{Name,self()}}),
    {stop,noreply,[]};
handle_call({opendoor,Button},_From,State) ->
    Cur = get(currentposition),
    case Cur == Button of
	true ->
	    io:format("Opening the door..."),
	    {reply,ok,State,{continue,move}};
	false ->

	    {reply,no,State}
    end;
%% {ok,noreply,State};

%% handle_call({serve_user,Floor, Dest},From,State) ->
%% Cur = get(current_position),
%% case Cur > Floor of
%% true ->
%% add_stop(down,Floor);
%% false ->

%% add_stop(up,Floor)

%% end,
%% case Cur > Dest of
%% true ->
%% add_stop(down,Dest);
%% false ->

%% add_stop(up,Dest)

%% end,
%% %%move(),
%% {reply,ok,State,{continue,move}};
handle_call({add_upstops,Stop},_From,State) ->
    Cur = get(current_position),
    case Cur > Stop of
	true ->
	    add_stop(down,Stop);
	false ->

	    add_stop(up,Stop)

    end,
    {reply,ok,State}.

handle_continue(move,State) ->
    move(),
    gen_event:notify(?LC,{info,curfloor,self(),get(current_position)}),
    gen_event:notify(?LC,{info, dire,self(),get(moving_direction)}),
    {noreply,State,100}.

handle_cast({info},State) ->
    A=[{Key,get(Key)}||Key <- get_keys()],
    io:format("Lift state:~p : ~p~n",[self(),A]),
    {noreply,State};
handle_cast({serve_user,Floor,Dest},State) ->
    Ground = get(ground),
    Top = get(top),
    Cur = get(current_position),
    case Floor >= Ground andalso Floor =< Top of
	true ->
	    case Cur > Floor of
		true ->
		    add_stop(down,Floor);
		false ->

		    add_stop(up,Floor)

	    end;
	false ->
	    io:format("Floor number should be in the range~n")
    end,
    case Dest >= Ground andalso Dest=< Top of
	true ->
	    case Cur > Dest of
		true ->
		    add_stop(down,Dest);
		false ->

		    add_stop(up,Dest)

	    end;
	false ->
	    io:format("Floor number should be in the range~n")
    end,

    {noreply,State,{continue,move}};
handle_cast({stop,Name},_State) ->
    io:format("Stoping lift ~p~n",[Name]),
    gen_event:sync_notify(?LC,{removelift,{Name,self()}}),
    {stop,normal,[]};

handle_cast({info,Key},State) ->
    A = get(Key),
    io:format("Lift ~p state: ~p~n",[Key,A]),
    {noreply,State}.
handle_info(timeout,State) ->
    case [] == get(ustops) andalso [] == get(dstops) of
	true ->
	    put(moving_direction,idle),
	    notify_controller(),
	    {noreply,State};
	false ->
	    move(),
	    notify_controller(),
	    {noreply,State,100}
    end.

terminate(Reason,_) ->
    io:format("Lift process closed with reason ~p ~n",[Reason]),
    ok.

notify_controller() ->
    gen_event:notify(?LC,{info,curfloor,self(),get(current_position)}),
    gen_event:notify(?LC,{info, dire,self(),get(moving_direction)}).
%% terminate(,) ->
%% ok.
%% handle_call({From,info,Key},State) ->
%% A = get(Key),
%% From!A;
%% %%io:format("Lift ~p state: ~p~n",[Key,A]),

%% handle_call({add_downstops,Stop},State) ->
%% Cur = get(current_position),
%% case Cur < Stop of
%% true ->
%% add_stop(up,Stop);

%% false ->
%% add_stop(down,Stop)
%% end;
%% handle_call({dest,Req},State) ->
%% Cur = get(current_position),
%% case Cur > Req of
%% true ->
%% add_stop(down,Req);
%% false ->
%% add_stop(up,Req)
%% end;

%% handle_call({exit},State) ->
%% io:format(Stoping the lift);

%% handle_call(Other,State) ->
%% io:format("Implementing .~p~n",[Other]),
%% {ok,State}.
%% lift()

%% after 100 ->
%% move(),1
%% lift()

%% end.

%% add_ustops([],,) ->
%% io:format(All requested upward floors are served...~n);
%% add_ustops([H|T],Cur,Speed) ->
%% Cur1 = moving_up(H,Cur,Speed),
%% add_ustops(T,Cur1,Speed).

%% add_dstops([],,) ->
%% io:format(All requested downward floors are served...~n);
%% add_dstops([H|T],Cur,Speed) ->
%% Cur1 = moving_down(H,Cur,Speed),
%% add_dstops(T,Cur1,Speed).

add_stop(up,Floor) ->
    case get(ustops) of
	[] ->
	    put(ustops,[Floor]);
	List when is_list(List) ->
	    put(ustops,lists:usort([Floor | List]));
	_ ->
	    io:format(error)
    end;

add_stop(down,Floor) ->
    case get(dstops) of
	[] ->
	    %%move(),%%remove
	    put(dstops,[Floor]);
	List when is_list(List) ->
	    put(dstops,lists:reverse(lists:usort([Floor | List])));
	_ ->
	    io:format(error)
    end.

move() ->
    %% M = get(moving_direction),
    %% io:format(direction--->~p~n,[M]),
    case get(moving_direction) of
	idle ->
	    case [] == get(ustops) andalso [] == get(dstops) of
		true ->
		    put(moving_direction,idle);

		false ->
		    case get(ustops) of %%FIX ME cur post
			[] ->
			    put(moving_direction,down),
			    move();

			_ ->
			    put(moving_direction,up),
			    move()
		    end,
		    case get(dstops) of %%FIX ME cur post
			[] ->
			    put(moving_direction,up),
			    move();

			_ ->
			    put(moving_direction,down),
			    move()
		    end

	    end;
	up ->
	    case get(ustops) of 
		[] -> 
		    %% Top = get(top),
		    put(moving_direction,down);
		%% put(current_position,Top);
		List=[F|L] when is_list(List) ->
		    Speed=get(speed),
		    Cur=get(current_position),   
		    timer:sleep(Speed),
		    case Cur == F of
			true ->
			    io:format("Opening door..~p ~n",[self()]),
			    put(ustops,L),
			    put(moving_direction,down);
			false ->
			    io:format("UP :~p:  ~p ~n",[self(),Cur+1]),
			    case Cur+1 == F of
				true ->
				    io:format("Opening door..~p ~n",[self()]),
				    put(ustops,L),
				    put(current_position,Cur+1);
				false ->
				    put(current_position,Cur+1)
			    end,
			    case Cur+1==get(top) of
				true ->   put(moving_direction,down);
				false -> ok
			    end
		    end;



		_ ->
		    io:format("error")
	    end;       
	down ->
	    case get(dstops) of 
		[] -> 
		    put(moving_direction,up);

		List=[F|L] when is_list(List) ->
		    Speed=get(speed),
		    Cur=get(current_position),
		    timer:sleep(Speed),
		    case Cur == F of
			true ->
			    io:format("Opening door..~p ~n",[self()]),
			    put(dstops,L),
			    put(moving_direction,up);
			false ->
			    io:format("Down ~p: ~p ~n",[self(),Cur-1]),
			    case Cur-1==F of
				true ->
				    io:format("Opening door..~p ~n",[self()]),
				    put(dstops,L),
				    put(current_position,Cur-1);
				false ->
				    put(current_position,Cur-1)
			    end,      
			    case Cur-1 == get(ground) of
				true -> put(moving_direction,up);
				false -> ok
			    end            
		    end;

		_ ->
		    io:format("error")
	    end;
	_ ->
	    io:format("Error in direction fetching..~n")
    end.
%% update_upstops(Req) ->
%% Cur = get(current_position),
%% io:format(Cur ~p~n,[Cur]),
%% case Req == Cur of
%% true ->
%% put(req_pos,Req),
%% io:format(Opening Door.. ~n),
%% liftcontroller();
%% false ->
%% Speed=get(speed),
%% case Req > Cur of
%% true ->
%% moving_up(Req,Cur,Speed),
%% %Accepting the user destination stop

%% liftcontroller();
%% false ->
%% moving_down(Req,Cur,Speed),
%% liftcontroller()
%% end
%% end.

%% update_downstops(Req) ->
%% Cur = get(current_position),
%% io:format(Cur ~p~n,[Cur]),
%% case Req == Cur of
%% true ->
%% put(req_pos,Req),
%% io:format(Opening Door.. ~n),
%% liftcontroller();
%% false ->
%% Speed=get(speed),
%% Dstops = sort(get(dstops)),
%% case Req < Cur of
%% true ->
%% moving_down(Req,Cur,Speed),
%% %Accepting the user destination stop

%% liftcontroller();
%% false ->
%% moving_up(Req,Cur,Speed),
%% liftcontroller()
%% end
%% end.%% move_up(Req) ->
%% Speed=get(speed),
%% Cur=get(current_position),

%% io:format(UFloor:~p Cur: ~p ~n,[Req,Cur]),
%% moving_up(Req,Cur,Speed).

%% moving_up(Pos,Pos,) ->
%% io:format(OPening Door..~n),
%% Pos;
%% moving_up(Req,Cur,Speed) ->
%% timer:sleep(Speed),
%% put(currentposition,Cur+1),
%% io:format(UFloor:~p Cur: ~p ~n,[Req,Cur]),
%% moving_up(Req,Cur+1,Speed).

%% move_down(Req) ->
%% Speed=get(speed),
%% Cur=get(current_position),

%% moving_down(Req,Cur,Speed).

%% moving_down(Pos,Pos,) ->
%% io:format(OPening Door..~n);
%% moving_down(Req,Cur,Speed) ->
%% timer:sleep(Speed),
%% put(currentposition,Cur-1),
%% io:format(DFloor:~p Cur:~p~n,[Cur,Req]),
