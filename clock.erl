%%%-------------------------------------------------------------------
%%% @author Thomas Arts <thomas@ThomasComputer.lan>
%%% @copyright (C) 2017, Thomas Arts
%%% @doc
%%%
%%% @end
%%% Created : 25 Jul 2017 by Thomas Arts <thomas@ThomasComputer.lan>
%%%-------------------------------------------------------------------
-module(clock).

-behaviour(gen_server).

%% API
-export([start_link/0, ms/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {}).

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

ms() ->
  gen_server:call(?SERVER, time).

init([]) ->
  {ok, #state{}}.

handle_call(time, _From, State) ->
  {_, _, Ms} = os:timestamp(),
  {reply, trunc(1/(1/Ms)), State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

