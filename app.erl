%%%-------------------------------------------------------------------
%%% @author Thomas Arts <thomas@ThomasComputer.lan>
%%% @copyright (C) 2017, Thomas Arts
%%% @doc
%%%
%%% @end
%%% Created : 25 Jul 2017 by Thomas Arts <thomas@ThomasComputer.lan>
%%%-------------------------------------------------------------------
-module(app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
  case sup:start_link() of
    {ok, Pid} ->
      {ok, Pid};
    Error ->
      Error
  end.

stop(_State) ->
  ok.
