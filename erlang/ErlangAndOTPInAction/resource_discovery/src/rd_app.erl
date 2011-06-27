-module(rd_app).

-behavior(application).

-export([start/2,
         stop/1]).

start(_StartType, _StartArgs) ->
    case rd_sup:start_link() of
        {ok, Pid} ->
            {ok, Pid};
        Error ->
            {error, Error}
    end.

stop(_State) ->
    ok.
