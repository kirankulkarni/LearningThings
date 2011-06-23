-module(sc_sup).

-behavior(supervisor).


%% API 
-export([start_link/0
        ]).

%% Behavior callback
-export([init/1]).


-define(SERVER, ?MODULE).

%% Called by Application to start the supervisor
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).


%% Callback for Supervisor Behavior, 
%% Should return {ok, {RestartStrategy, ChildSpec}}
init([]) ->
    ElementSuperVisor = {sc_element_sup, {sc_element_sup, start_link, []},
                         permanent, 2000, supervisor, [sc_element_sup]},
    EventManager = {sc_event, {sc_event, start_link, []},
                    permanent, 2000, worker, [dynamic]},
    ModStats = {mod_stats, {mod_stats, start_link, []},
                    permanent, 2000, worker, [mod_stats]},
    Children = [ElementSuperVisor, EventManager, ModStats],
    RestartStrategy = {one_for_one, 4, 3600},
    {ok, {RestartStrategy, Children}}.
