%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Author: Kiran Kulkarni %%
%% Date:                  %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(simple_cache).

-export([create/2,
        fetch/1,
        delete/1,
        get_stats/0]).

create(Key, Value) ->
    case sc_store:lookup(Key) of
        {ok, Pid} ->
            sc_element:replace(Pid, Value),
            sc_event:replace(Key, Value);
        {error, not_found} ->
            {ok, Pid} = sc_element:create(Value),
            sc_store:insert(Key, Pid),
            sc_event:create(Key, Value)
    end.

fetch(Key) ->
    try
        {ok, Pid} = sc_store:lookup(Key),
        {ok, Value} = sc_element:fetch(Pid),
        sc_event:lookup(Key),
        {ok, Value}
    catch
        _Class:_Exception ->
            {error, not_found}
    end.

delete(Key) ->
    case sc_store:lookup(Key) of
        {ok, Pid} ->
            sc_element:delete(Pid);
        {error, not_found} ->
            {error, not_found}
    end.
            
get_stats() ->
    mod_stats:get_cache_stats().
