-module(named_process).
-compile(export_all).
%Pid=named_process:start_critic()
%named_process:judge(Pid,"Johnny Crash","The Token Ring of Fire")
%exit(Pid,solar_storm)
start_critic() ->
  spawn(?MODULE, critic, []).
 
judge(Pid, Band, Album) ->
   Pid ! {self(), {Band, Album}},
   receive
      {Pid, Criticism} -> {Pid,Criticism}
   after 2000 ->
      timeout
   end.
 
critic() ->
   receive
      {From, {"Rage Against the Turing Machine", "Unit Testify"}} ->
         From ! {self(), "They are great!"};
      {From, {"System of a Downtime", "Memoize"}} ->
         From ! {self(), "They're not Johnny Crash but they're good."};
      {From, {"Johnny Crash", "The Token Ring of Fire"}} ->
         From ! {self(), "Simply incredible."};
      {From, {_Band, _Album}} ->
         From ! {self(), "They are terrible!"}
   end,
   critic().
