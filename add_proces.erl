-module(add_process).
-compile(export_all).
add_process(A,B)->
    receive
     add->A+B
    end
