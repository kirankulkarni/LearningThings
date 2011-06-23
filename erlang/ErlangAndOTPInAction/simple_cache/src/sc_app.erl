-module(sc_app).

-behavior(application).

-export([start/2,
         stop/1]).


start(_StartType, _StartArgs) ->
    sc_store:init(),
    case sc_sup:start_link() of
        {ok, Pid} ->
            {ok, Pid};
        Error ->
            {error, Error}
    end.


stop(_State) ->
    ok.
