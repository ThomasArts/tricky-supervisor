%%%-------------------------------------------------------------------
%%% @author Thomas Arts <thomas@ThomasComputer.lan>
%%% @copyright (C) 2017, Thomas Arts
%%% @doc
%%%
%%% @end
%%% Created : 25 Jul 2017 by Thomas Arts <thomas@ThomasComputer.lan>
%%%-------------------------------------------------------------------
-module(sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->

  SupFlags = #{strategy => one_for_one,
               intensity => 1,
               period => 5},

  Children = 
    [ #{id => clock, start => {clock, start_link, []}},
      #{id => gui, start => {gui, start_link, []}}
    ],

  {ok, {SupFlags, Children}}.

