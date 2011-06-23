-module(sc_store).

-export([init/0,
         insert/2,
         lookup/1,
         delete/1
        ]).


-define(TABLE_ID, simple_cache).


init() ->
    ets:new(?TABLE_ID, [public, named_table, set]),
    ok.

insert(Key, Pid) ->
    ets:insert(?TABLE_ID, {Key, Pid}).

lookup(Key) ->
    case ets:lookup(?TABLE_ID, Key) of
        [{Key, Pid}] ->
            {ok, Pid};
        [] ->
            {error, not_found}
    end.

delete(Pid) ->
    case ets:match(?TABLE_ID, {'$1', Pid}) of
        [[Key]] ->
            sc_event:delete(Key),
            ets:delete(?TABLE_ID, Key);
        [] ->
            true
    end.
            
