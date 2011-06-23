-module(sc_element).

-behavior(gen_server).

%% API
-export([start_link/2,
         create/2,
         create/1,
         fetch/1,
         delete/1,
         replace/2
        ]).

%% gen_server callback
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(DEFAULT_LEASE_TIME, (60 * 60 * 24)).

-record(state, {value, lease_time, start_time}).


start_link(Value, LeaseTime) ->
    gen_server:start_link(?MODULE, [Value, LeaseTime], []).

create(Value, LeaseTime) ->
    sc_element_sup:start_child(Value, LeaseTime).

create(Value) ->
    create(Value, ?DEFAULT_LEASE_TIME).

fetch(Pid) ->
    gen_server:call(Pid, fetch).

delete(Pid) ->
    gen_server:cast(Pid, delete).

replace(Pid, Value) ->
    gen_server:cast(Pid, {replace, Value}).

%% gen_server callbacks
init([Value, LeaseTime]) ->
    Now = get_time(),
    State = #state{value = Value,
                   start_time = Now,
                   lease_time = LeaseTime},
    {ok, State, time_left(Now, LeaseTime)}.

handle_call(fetch, _From, State) ->
    #state{value = Value,
           start_time = StartTime,
           lease_time = LeaseTime} = State,
    {reply, {ok, Value}, State, time_left(StartTime, LeaseTime)}.

handle_cast({replace, Value}, State) ->
    #state{start_time = StartTime,
           lease_time = LeaseTime} = State,
    {noreply, State#state{value=Value}, time_left(StartTime, LeaseTime)};
handle_cast(delete, State) ->
    {stop, normal, State};
handle_cast(stop, State) ->
    {stop, normal, State}.

handle_info(timeout, State)->
    io:format("Timeout occoured on  Proces ~p~n", [self()]),
    {stop, normal, State}.

terminate(Reason, _State)->
    io:format("Terminating the Proces ~p, Reason: ~p~n", [self(), Reason]),
    sc_store:delete(self()).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% util functions
get_time() ->
    Now = calendar:universal_time(),
    calendar:datetime_to_gregorian_seconds(Now).

time_left(_StartTime, infinity) ->
    infinity;
time_left(StartTime, LeaseTime) ->
    Now = get_time(),
    TimeElapsed = Now - StartTime,
    case LeaseTime - TimeElapsed of
        Time when Time > 0 ->
            Time * 1000;
        _Time -> 0
    end.

