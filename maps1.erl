-module(maps1).
-export([maps_op/1,maps_op1/3]).
maps_op(M)->
  Pred = fun(K,V) -> is_atom(K) andalso (V rem 2) =:= 0 end,
  maps:filter(Pred,M).
%enter map,listoftuples,key(to_check)
maps_op1(M,L,Key)->
     Fun = fun(K,V,AccIn) when is_list(K) -> AccIn + V end,%fun(key,val,Accu)
     io:fwrite("The sum of values is:~p~n",[maps:fold(Fun,0,M)]),%maps:fold(function,init,Map_iter)
     io:fwrite("Conversion of list of tuples is:~p~n",[maps:from_list(L)]),%converts a list of tuples to map,if keys are duplicated then latter one is considered !    
     io:fwrite("Conversion of map to list of tuples:~p~n",[maps:to_list(M)]),
     io:fwrite("The value of Key is:~p~n",[maps:find(Key,M)]),%Finds the key in M and returns its value
     io:fwrite("The value of Key is:~p~n",[maps:get(Key,M,"null")]),%Finds the key in M and if Key is not present then returns default value "null"
     io:fwrite("This key is present in the given map(true/false):~p~n",[maps:is_key(Key,M)]),
     io:fwrite("The list of keys is:~p~n",[maps:keys(M)]),
     io:fwrite("The list of values is:~p~n",[maps:values(M)]),
      
     M1=#{"b"=>67,"c"=>78},
     M2=maps:new(),%creates an empty map
     Fun1 = fun(K,V1) when is_list(K) -> V1*2 end,
     io:fwrite("Using map function ,values of map are doubled:~p~n",[maps:map(Fun1,M1)]),
     io:fwrite("Merging of maps results in:~p~n",[maps:merge(M,M1)]),
     io:fwrite("Putting a new pair results in:~p~n",[maps:put("c",47,M2)]),
     io:fwrite("Removing a key from map results in:~p~n",[maps:remove("c",M2)]),
     io:fwrite("Size of map is:~p~n",[maps:size(M)]),
     io:fwrite("Taking a key from map :~p~n",[maps:take("a",M)]),%Take returns value of key if it exists otherwise return error
     io:fwrite("Taking a key from map:~p~n",[maps:take("does not exist",M)]),
     io:fwrite("Updating of existing key:~p~n",[maps:update("a",56,M)]),
     Ks=["a","c","d"],
     io:fwrite("New map WITH mentioned keys:~p~n",[maps:with(Ks,M)]),
     io:fwrite("New map WITHOUT mentioned keys:~p~n",[maps:without(Ks,M)]).
    
    
    
 
    
    
     
