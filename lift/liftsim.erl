-module(liftsim).
-export([start/0,req/0,reqs/1,req1/0]).
-define(LC,liftcontroller).
-define(L,lift).

start()->
    ?LC:start(),
    ?L:start({a,0,10,10,4000}),
    ?L:start({b,0,10,20,4000}),
    ?L:start({c,0,10,30,4000}),
    ?L:start({d,0,10,40,4000}),
    ?LC:info().
req()->
   gen_event:notify({global,?LC},{req,5,2}),
   ?LC:info(),
   gen_event:notify({global,?LC},{req,6,1}),
   ?LC:info(),
   gen_event:notify({global,?LC},{req,4,3}),
   ?LC:info().
 
req1()->
   gen_event:notify({global,?LC},{req,5,2}).


reqs(Req)->
   gen_event:sync_notify({global,?LC},Req).   
