{application, resource_discovery,
 [{description, "Simple Resource Discovery demo app"},
  {vsn, "0.1.0"},
  {modules, [rd_app,
             rd_sup,
             rd_server]},
  {registered, [rd_sup, rd_server]},
  {applications, [kernel, stdlib]},
  {mod, {rd_app, []}}
 ]
}.
