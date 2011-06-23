-module(mod_stats).

-behavior(gen_server).

%% gen_server Callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%% API
-export([start_link/0,
         get_cache_stats/0,
         update_stats/1]).


-define(SERVER, ?MODULE).

-record(state, {number_of_keys, insertions, deletions, modifications}).

%% API IMPLEMENTATION
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

get_cache_stats() ->
    gen_server:call(?SERVER, get_stats).

update_stats(key_added) ->
    gen_server:cast(?SERVER, key_added);
update_stats(key_modified) ->
    gen_server:cast(?SERVER, key_modified);
update_stats(key_deleted) ->
    gen_server:cast(?SERVER, key_deleted).

%% gen_server Callbacks
init(_Args) ->
    mod_stats_event_hooks:add_handler(),
    State = #state{number_of_keys = 0,
                   insertions = 0,
                   deletions = 0,
                   modifications = 0},
    {ok, State}.

%% 
handle_call(get_stats, _From, State) ->
    #state{number_of_keys = Keys,
           insertions = Insertions,
           deletions = Deletions,
           modifications = Modifications} = State,
    Result = [{number_of_keys, Keys},
              {insertions, Insertions},
              {deletions, Deletions},
              {modifications, Modifications}],
    {reply, {ok, Result}, State}.

%% 
handle_cast(key_added, #state{number_of_keys=Keys, insertions=Insertions} = State) ->
    NewState = State#state{number_of_keys= (Keys + 1),
                           insertions = (Insertions + 1)},
    {noreply, NewState};
handle_cast(key_deleted, #state{number_of_keys=Keys, deletions=Deletions} = State) ->
    NewState = State#state{number_of_keys = (Keys - 1),
                           deletions = (Deletions + 1)},    
    {noreply, NewState};
handle_cast(key_modified, #state{modifications=Modifications} = State) ->
    NewState = State#state{modifications = (Modifications + 1)},
    {noreply, NewState};
handle_cast(stop, State) ->
    {stop, shutdown, State}.

%%
handle_info(_Info, State) ->
    {noreply, State}.

%%
terminate(_Reason, _State) ->
    mod_stats_event_hooks:delete_handler(),
    ok.

%%
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


    
