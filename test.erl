%%% @author Thomas Arts <thomas@ThomasComputer.lan>
%%% @copyright (C) 2017, Thomas Arts
%%% @doc
%%%
%%% @end
%%% Created : 25 Jul 2017 by Thomas Arts <thomas@ThomasComputer.lan>

-module(test).

-compile(export_all).


%% Test 1000 succeeds (normally, test 5M fails)
test(N) ->
  ok = application:ensure_started(app),
  timer:sleep(10),
  repeat(N).

repeat(0) -> 
  gui:digits();
repeat(N) ->
  gui:digits(), 
  repeat(N-1).
  
