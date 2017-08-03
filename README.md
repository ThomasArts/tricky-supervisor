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


## The clock and gui callback

Just to be OTP compliant, the two processes clock and gui are defined
as gen_servers. The clock has beside start and stop the API call
clock:ms(), which should return the milliseconds of os:timestamp.

After starting the application, this looks as follows:
```erlang
9> {os:timestamp(), clock:ms()}.
{{1501,766116,703340},703358}
10> {os:timestamp(), clock:ms()}.
{{1501,766119,55250},55268}
11> {os:timestamp(), clock:ms()}.
{{1501,766122,487512},487529}
```

The first value in the above tuples is a direct call to
os:timestamp(), the second call to clock:ms() returns only the
milliseconds.

The gui process has an API call that can ask for the digits that the
millisecond dial has shown so far. There are at most ten digits, thus
the longer the clock runs, the more likely it is that all digits have
been displayed. **It's contrived...**

```erlang
15> application:start(app). 
ok
16> gui:digits().          
[0]
17> gui:digits().
[0]
18> gui:digits().
[0,3]
19> gui:digits().
[0,3]
20> gui:digits().
[0,3,8]
21> gui:digits().
[0,3,6,8]
22> gui:digits().
[0,3,6,8]
```

It takes a bit of time before we hit all digits, due to rounding and
timing. But if we call the function often enough (20 times or so), we
get them all.

## Some super naive testing

Although testing without QuickCheck is unrealistic, I present a kind
of unit test here:

```erlang
test(N) ->
  ok = application:ensure_started(app),
  timer:sleep(10),
  repeat(N).

repeat(0) -> 
  gui:digits();
repeat(N) ->
  gui:digits(), 
  repeat(N-1).
  ```

Just call gui:digits() a number of times.
The result is a list with a subset of the digits.
As said before, when call the API 20 times, we get them all, when we
call it 6 times, we get a subset:

```erlang
49> application:stop(app).

=INFO REPORT==== 3-Aug-2017::15:27:45 ===
    application: app
    exited: stopped
    type: temporary
ok
50> test:test(20).        
[0,1,2,3,4,5,6,8,9]
51> application:stop(app).

=INFO REPORT==== 3-Aug-2017::15:27:58 ===
    application: app
    exited: stopped
    type: temporary
ok
52> test:test(6).        
[3,5,6,8]
```

## Running many tests

If we now run many tests, say calling the same gui:digits() 10,000
times, or 100,000 times?

```erlang
54> test:test(10000).     
[0,1,2,3,4,5,6,7,8,9]
55> test:test(100000).
[0,1,2,3,4,5,6,7,8,9]
```

Or even as much as 5 million times!

```erlang
56> test:test(5000000).

=ERROR REPORT==== 3-Aug-2017::15:30:35 ===
** Generic server clock terminating 
** Last message in was time
** When Server state == {state}
** Reason for termination == 
** {badarith,[{clock,handle_call,3,[{file,"clock.erl"},{line,35}]},
              {gen_server,try_handle_call,4,
                          [{file,"gen_server.erl"},{line,615}]},
              {gen_server,handle_msg,5,[{file,"gen_server.erl"},{line,647}]},
              {proc_lib,init_p_do_apply,3,
                        [{file,"proc_lib.erl"},{line,247}]}]}
                        ...
=INFO REPORT==== 3-Aug-2017::15:30:35 ===
    application: app
    exited: shutdown
    type: temporary

```

Wow, that's a surprise. We did hit a very rare error, after 5 million
calls to gui:digits() the server crashed.

## Rare errors

But if the error is so rare, how then did it hit it twice in 5
seconds? Actually, did the test take 5 seconds at all, probably not on
your new machine.

Still, the supervision tree should restart the crashing process and
since it happens very seldomly, **why then would it actually crash the
supervisor itself in order to kill the application?**

The reason is: gui and clock depend on each other. In fact, the clock
process is the one that crashes, but it takes the gui with it. This is
due to the fact that the gui calls the clock. If the clock crashes,
the gui crashes and that is together 2 crashes within 5 seconds.

**I guess this is a common pitfall and you need to inspect your
application for it.** Make sure that the children of the supervision
tree are independent. Use code inspection or a QuickCheck model for
doing so. 





