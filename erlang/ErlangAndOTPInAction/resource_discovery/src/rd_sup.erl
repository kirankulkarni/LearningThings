-module(rd_sup).

-behavior(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SUPERVISOR, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SUPERVISOR}, ?MODULE, []).

init(_Args) ->
    RDServer = {rd_server, {rd_server, start_link, []},
                permanent, 2000, worker, [rd_server]},
    RDScheduler = {rd_scheduler, {rd_scheduler, start_link, []},
                   permanent, 2000, worker, [rd_scheduler]},
    RestartStrategy = {one_for_one, 4, 3600},
    Children = [RDServer, RDScheduler],
    {ok, {RestartStrategy, Children}}.

