-module(liftcontroller).
%%API functions
-export([start/0,info/0,stop/0]).
-behaviour(gen_event).
%%call back functions
-export([init/1,handle_event/2,handle_call/2,terminate/2]).
-define(LC,{global,?MODULE}).

start() ->
    {ok,PID} = gen_event:start({global,?MODULE}),
    gen_event:add_handler(PID,?MODULE,[]),
       io:format("Liftcontroller started [PID= ~p] ~n",[PID]).

info() ->
    gen_event:notify(?LC,info).
stop() ->
    gen_event:delete_handler(?LC,?MODULE,{userstop,normal}),
    gen_event:stop(?LC).

terminate({userstop,_Reason},_Sta) ->
    case get(lifts) of
	[] ->
	    io:format("No lift is present to stop..~n");
	List ->
	    io:format("Stopping lifts~n"),
            [lift:stop(A)||{A,_}<-List]
    end.
init(State=[]) ->
    io:format("Initializing the controller...~n"),
    put(lifts,[]),
    put(floors,[]),
     {ok,State}.
%%button press from inside panel
%% handle_event({bpil,FButton},State) ->
%% case [L||L<-get(lifts),get({L,curfloor})==FButton] of
%% [{Name,PID}] ->
%% case gen_server:call(Name,{opendoor,FButton}) of
%% ok ->
%% {ok,State};
%% no ->
%% %%handle the ambiguity
%% {ok,State}
%% end;
%% [] ->
%% Pid=best_lift(FButton),
%% gen_server:call(Pid,{serve_user,FButton})
%% end,
%% {ok,State};
%%Floor panel input
handle_event({req,Floor,Dest},State) ->
    %% Dire=
    %% if Floor > Dest -> down;
    %% true -> up
    %% end,
    Pid=best_lift(Floor,Dest),
    io:format("Optimum Match..->~p~n",[Pid]),
    gen_server:cast(Pid,{serve_user,Floor,Dest}),%%Fix ME
    timer:sleep(10000),%%FIX
    {ok,State};

handle_event({info,Key,LiftPid,Value},State) ->
    case [L||L={_,Pid} <-get(lifts),Pid==LiftPid] of
	[L] ->
	    put({L,Key},Value);
	[] ->
	    ok
    end,
    {ok,State};

handle_event({newlift,PID},State) ->
    case get(lifts) of
	Cur ->
	    put(lifts,[PID|Cur]),
	    put({PID,dire},idle),
	    put({PID,curfloor},0),
	    put({PID,restrictedfloors},[])
    end,
    io:format("New Lift registered: ~p~n",[PID]),
    {ok,State};

handle_event({removelift,Lift},State) ->
    case get(lifts) of
	Cur ->
	    put(lifts,[Li||Li<-Cur,Lift=/=Li]),
	    erase({Lift,dire}),
	    erase({Lift,curfloor}),
	    erase({Lift,restrictedfloors})
    end,
    io:format("Removal of the Lift : ~p~n",[Lift]),
    {ok,State};

handle_event(info,State) ->
    A=[{Key,get(Key)}||Key <- get_keys()],
    io:format("Liftcontroller state: ~p~n",[A]),
    {ok,State}.

handle_call({liftinfo,Name},State) ->
    Lift = get({lift,Name}),
    io:format("Lift info ~p~n",[Lift]),
    {ok,{liftinfo, Lift},State}.

%% terminate(,) ->
%% stop.

%%Internal Functions
%% best_lift(Button) ->
%% Lifts =get(lifts),
%% case [L||L<-Lifts,get({L,curfloor})==Button] of
%% [{Name,PID}] ->
%% PID;
%% More ->
%% {,PID} = hd(More);
%% [] ->
%% CurF=[get({L,curfloor})||L<-Lifts],
%% nearest_value(Button,CurF);
%% _->
%% ok
%% end.
%%(2,[0,3,5,7])
nearest_idle(_,[]) ->
    [];
nearest_idle(CurFloor,[{{_,_},H}|T]) ->
    [abs(CurFloor-H)|nearest_idle(CurFloor,T)].

nearest_idle_eve(CurFloor,Ls1) ->
    Mn=lists:min(nearest_idle(CurFloor,Ls1)),
    [Ls||Ls={{_Name,_Pid},Cur} <- Ls1,abs(CurFloor-Cur)==Mn].

nearest(_,[],Acc) ->
    Acc;
%% nearest(CurFloor,[{{,},CurFloor}|T],Acc) ->

%% nearest(CurFloor,T,);
nearest(Dire,[Acc|T],[]) ->

    nearest(Dire,T,Acc);

nearest(Dire,[{{_,_},Cur}|T],Acc={{_,_},Cur}) ->

    nearest(Dire,T,Acc);
%5 6 10
nearest(Dire,[Acc={{_,_},H}|T],Acc1={{_,_},Cur}) ->
    Fin=case Dire of
	    down ->
		if H < Cur -> Acc;
		   true -> Acc1
		end;
	    up ->
		if H > Cur -> Acc;
		   true -> Acc1
		end
	end,

    nearest(Dire,T,Fin).

nearest_value(Dire,Ls1) ->
    nearest(Dire,Ls1,[]).

%% nearest_value(Val,List) ->
%% 5.
best_lift(Floor,Dest) ->
    Dire=
	if Floor > Dest -> down;
	   true -> up
	end,
    best_lift(Floor,Dest,Dire).
%UC1 = 5 2 down [5,6,7,8,9,10]
%UC2 = 2 5 up [0,1,2]
%[{{a,PID},7},{{b,PID},8}]
best_lift(Floor,_Dest,Dire) -> %%FIX me Consider the dest to derive best elevator
    Lifts = get(lifts),
    %%put(moving_direction,Dire),
    io:format("get lifts ...~p~n",[Lifts]),
    %%A= [L||L<-Lifts,get({L,dire})Dire,get({L,curfloor}) == Floor],
    %%io:format(value of case statement..~p~n,[A]),
    SameDire=
	case [L||L<-Lifts,get({L,dire})==Dire,get({L,curfloor}) == Floor] of
	    [] ->
		case Dire of
		    down ->
			io:format("get lifts down...~p~n",[Lifts]),
			[L||L<-Lifts,get({L,dire})==Dire,get({L,curfloor}) > Floor];
		    up ->
			io:format("get lifts up...~p~n",[Lifts]),
			[L||L<-Lifts,get({L,dire})==Dire,get({L,curfloor}) < Floor]

		end;
	    %% [H|T] ->
	    %%  io:format("get lifts ...~p~n",[Lifts]),
	    %%  Lis = lists:append([T,[H]]),
	    %%  io:format("get lifts after append ...~p~n",[Lis]),
	    %%  put(lifts,Lis),
	    %%  H
	    Num ->
		hd(Num)
	end,

    case SameDire of
	[] ->
	    %IDLE lift selection
	    case [L||L<-Lifts,get({L,dire})==idle] of
		[] ->
		    %% case [L||L<-Lifts,get({L,dire})==up,get({L,curfloor}) >= Floor] of%%Consider the UPstops
		    %%  [] ->
		    %%      ok;
		    %%  [{Name,PID}] ->
		    %%      PID;
		    %%  List ->
		    %%      CurF=[{L,get({L,curfloor}})||L<-List],
		    %%      {{Name,Elevator},_} = nearest_value(Dire,CurF), %%FIX ME
		    %%      Elevator
		    %% end;
		    ok;
		[{_,PID}] ->
		    PID;
		List ->
		    CurF=[{L,get({L,curfloor})}||L<-List],
		    Elevator =  case nearest_idle_eve(Floor,CurF) of %%FIX ME idle state 
				    [{{_,IdleEve},_}] ->
					IdleEve;
				    Multi ->
					{{_,IdleEve},_}= hd(Multi),
					IdleEve
				end,
		    Elevator    
	    end;
	{_Name,PID} ->
	    PID;
	List ->
	    CurF=[{L,get({L,curfloor})}||L<-List],
	    {{_Name,Elevator},_} = nearest_value(Dire,CurF),
	    Elevator        
    end.
