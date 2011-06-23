{application, simple_cache,
 [{description, "A simple cache, written by KK"},
  {vsn, "0.1.0"},
  {modules, [
             sc_app,
             sc_sup
            ]},
  {registered, [sc_sup]},
  {applications, [kernel, stdlib]},
  {mod, {sc_app, []}}
 ]
}.
