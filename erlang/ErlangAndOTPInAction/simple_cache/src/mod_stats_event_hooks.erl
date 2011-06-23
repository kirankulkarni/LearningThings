-module(mod_stats_event_hooks).

-behavior(gen_event).

-export([init/1,
         handle_event/2,
         handle_call/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-export([add_handler/0,
         delete_handler/0]).

-record(state, {}).

add_handler() ->
    sc_event:add_handler(?MODULE, []).

delete_handler() ->
    sc_event:delete_handler(?MODULE, []).

%% Callback
init([]) ->
    {ok, #state{}}.

handle_event({lookup, Key}, State) ->
    io:format("Key ~p was fetched", [Key]),
    {ok, State};
handle_event({create, _Key, _Value}, State) ->
    mod_stats:update_stats(key_added),
    {ok, State};
handle_event({replace, _Key, _Value}, State) ->
    mod_stats:update_stats(key_modified),
    {ok, State};
handle_event({delete, _Key}, State) ->
    mod_stats:update_stats(key_deleted),
    {ok, State}.

handle_call(_Request, State) ->
    Reply = ok,
    {ok, Reply, State}.

handle_info(_Info, State) ->
    {ok, State}.

terminate(_Arg, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
