-module(kitchen).
-compile(export_all).
fridge2(FoodList) ->
receive
   {From, {store, Food}} ->
                           io:format("~p~n",[[Food|FoodList]]),
                           fridge2([Food|FoodList]);
   {From, {take, Food}} ->
     case lists:member(Food, FoodList) of
     true ->From ! {self(), {ok_taken, Food}},
            fridge2(lists:delete(Food, FoodList));
     false ->From ! {self(), not_found},
            fridge2(FoodList)
     end;
   terminate ->ok
end.
