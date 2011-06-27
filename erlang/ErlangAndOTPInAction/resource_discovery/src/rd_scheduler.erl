-module(rd_scheduler).

-behavior(gen_server).

%% Exports No API except start_link
-export([start_link/1,
         start_link/0]).

-export([init/1,
         handle_cast/2,
         handle_call/3,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SCHEDULAR, ?MODULE).
-define(DEFAULT_TIMOUT, (60 * 5)).

-record(state, {start_time, timeout}).

%% API
start_link(Timeout) ->
    gen_server:start_link({local, ?SCHEDULAR}, ?MODULE, [Timeout], []).
start_link() ->
    start_link(?DEFAULT_TIMOUT).

%% gen_server callback
init([Timeout]) ->
    StartTime = get_now(),
    State = #state{start_time = StartTime,
                   timeout = Timeout},
    {ok, State, get_timeout(StartTime, Timeout)}.

handle_cast(_, #state{start_time = StartTime,
                      timeout = Timeout} = State) ->
    {noreply, State, get_timeout(StartTime, Timeout)}.

handle_call(_, _From, #state{start_time = StartTime,
                             timeout = Timeout} = State) ->
    {reply, ok, State, get_timeout(StartTime, Timeout)}.

handle_info(timeout, #state{timeout=Timeout}) ->
    rd_server:trade_resources(),
    StartTime = get_now(),
    NewState = #state{start_time = StartTime,
                      timeout = Timeout},
    {noreply, NewState, get_timeout(StartTime, Timeout)};
handle_info(_Info, #state{start_time = StartTime,
                          timeout = Timeout} = State) ->
    {noreply, State, get_timeout(StartTime, Timeout)}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Utility functions
get_now() ->
    Now = calendar:universal_time(),
    calendar:datetime_to_gregorian_seconds(Now).

get_timeout(_StartTime, infinity) ->
    infinity;
get_timeout(StartTime, Timeout) ->
    CurrentTime = get_now(),
    TimeElapsed = CurrentTime - StartTime,
    case TimeElapsed < Timeout of
        true ->
            (Timeout - TimeElapsed) * 1000;
        false ->
            0
    end.
    

        


        



