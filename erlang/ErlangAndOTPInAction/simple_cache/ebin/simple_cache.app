{application, simple_cache,
 [{description, "A simple cache, written by KK"},
  {vsn, "0.2.0"},
  {modules, [
             sc_app,
             sc_sup,
             sc_element_sup,
             sc_store,
             sc_element,
             sc_event,
             mod_stats,
             mod_stats_event_hooks
            ]},
  {registered, [sc_sup]},
  {applications, [kernel, stdlib, sasl, mnesia, resource_discovery]},
  {mod, {sc_app, []}}
 ]
}.
