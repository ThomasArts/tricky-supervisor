{application, app,
             [{description,"Example Application"},
              {vsn,"0.1.0"},
              {registered,[clock, gui]},
              {mod,{app,[]}},
              {applications,[kernel,stdlib]},
              {env,[]},
              {modules,[app, sup, clock, gui]}
              ]}.
