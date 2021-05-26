-module(best_lift).
-import(lists,[min/1]). 
-export([best_lift/2,main/2]).
best_lift(_,[])->
      [];
best_lift(CurFloor,[H|T]) ->
      [abs(CurFloor-H)|best_lift(CurFloor,T)].
main(CurFloor,Ls1)->
    Mn=min(best_lift(CurFloor,Ls1)),
    [Ls||Ls<-Ls1,abs(CurFloor-Ls)==Mn].
    
