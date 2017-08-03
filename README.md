# Erlang and Elixir supervisor trickyness

Erlang and Elixir offer supervisor processes to make applications fault-tolerant. The simple idea is that at the top of an application one starts a supervisor process that monitors other processes in the application. If a process crashes, the supervisor can then restart part of the system instead of the complete system. For the user, certain faults are simply invisible, due to such restarts. Others are visible for one user, but have minimal impact for other users.

Some faults just happen almost never: a bit flip in hardware, a packet loss in the TCP stack, things like that. If such rare fault occurs, part of the system, but not the complete system may be affected. A good software design makes it possible to just restart the affected part and leave the rest running. 

When creating a QuickCheck model for the [GRiSP board](https://github.com/grisp/grisp) some tests simply killed the application. This is not very surprising, since if we provoke a fault with QuickCheck, we might likely provoke it several times, quickly after each other. But shrinking does not really work if the application is not running, so the property was changed to start and stop the application for each test. Now we could shrink and... by provoking 1 fault, the complete application stops working. This was a surprise and made me write this blog.

## A simple clock application

In order to demonstrate the point, I use a contrived example application with two generic servers controlled by a supervision tree. The application really does nothing other than fiddling a bit with os:timestamp/0.

### The application callback

We use a completely default application callback. The only thing it does is starting the supervision tree. 

```erlang
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
```

Together with it we also define the app.app data structure to tell Erlang OTP that indeed we have a little application at hand.

```erlang
{application, app,
             [{description,"Example Application"},
              {vsn,"0.1.0"},
              {registered,[clock, gui]},
              {mod,{app,[]}},
              {applications,[kernel,stdlib]},
              {env,[]},
              {modules,[app, sup, clock, gui]}
              ]}.
```

This is completely standard way of defining applications and only if the supervisor crashes, this application will be stopped (given that it is not configured to restart).

### The supervisor tree

The supervisor process will monitor two processes. A supervision strategy determines how this is done. We want each process to be independently restarted if it fails.

```erlang
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
```

From this supervisor restart strategy, you may derive that if one of the processes crashes, it is automatically restarted, without restarting the other process.
Moreover, you tolerate 1 failure per 5 seconds. Since 5 seconds is eternity in computer land, this means that the error actually is very unlikely to happen and that if it happens, just restarting may solve the majority of your problems.

## The gui callback

Just to be OTP compliant, the two processes clock and gui are defined as gen_servers. The gui process has an API call that can ask for the digits that the millisecond dial has shown so far. There are at most ten digits, thus the longer the clock runs, the more likely it is that all digits have been displayed. It's contrived...
I leave out the default lines that are not important, the code is in this repo.

```erlang
-module(gui).

-behaviour(gen_server).

%% API
-export([start_link/0, digits/0]).

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

```




