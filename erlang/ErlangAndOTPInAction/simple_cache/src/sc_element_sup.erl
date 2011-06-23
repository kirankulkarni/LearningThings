-module(sc_element_sup).

-behavior(supervisor).


%% API 
-export([start_link/0,
        start_child/2
        ]).

%% Behavior callback
-export([init/1]).


-define(SERVER, ?MODULE).

%% Called by Application to start the supervisor
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).


start_child(Value, LeaseTime) ->
    supervisor:start_child(?SERVER, [Value, LeaseTime]).


%% Callback for Supervisor Behavior, 
%% Should return {ok, {RestartStrategy, ChildSpec}}
init([]) ->
    Element = {sc_element, {sc_element, start_link, []},
               temporary, brutal_kill, worker, [sc_element]},
    Children = [Element],
    RestartStrategy = {simple_one_for_one, 0, 1},
    {ok, {RestartStrategy, Children}}.
