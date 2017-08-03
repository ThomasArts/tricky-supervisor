%%%-------------------------------------------------------------------
%%% @author Thomas Arts <thomas@ThomasComputer.lan>
%%% @copyright (C) 2017, Thomas Arts
%%% @doc
%%%
%%% @end
%%% Created : 25 Jul 2017 by Thomas Arts <thomas@ThomasComputer.lan>
%%%-------------------------------------------------------------------
-module(gui).

-behaviour(gen_server).

%% API
-export([start_link/0, digits/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

digits() ->
  gen_server:call(?SERVER, digits).


init([]) ->
  {ok, []}.

handle_call(digits, _From, State) ->
  LastDigit = clock:ms() rem 10,
  NewState =  lists:umerge(State, [LastDigit]),
  {reply, NewState, NewState}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

