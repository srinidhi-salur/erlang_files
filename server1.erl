-module(server1).
-export([start/0,start1/1]).
start1(Balance)->
     put(balance,Balance),
     start().
    
start()->
   receive
       {From,{deposit,D}}->
           Balance=get(balance),
           Amount=Balance+D,
           put(balance,Amount),
            From!{self(),{deposited,Amount}},
           start();
        {From,{checkbalance}}->
           Cb=get(balance),
           From!{self(),{checkedbalance,Cb}},
           start();
         {From,{withdraw,W}}->
            Balance=get(balance),
            Amount=Balance-W,
            put(balance,Amount),
            From !{self(),{withdrawn,Amount}},
            start()
     end.
