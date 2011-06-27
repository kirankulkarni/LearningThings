-module(rd_server).

-behavior(gen_server).

-export([start_link/0,
         fetch_resources/1,
         trade_resources/0,
         add_target_resource_type/1,
         add_local_resource/2]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {target_resource_types, local_resources, found_resources}).

%% API
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

add_target_resource_type(Type) ->
    gen_server:cast(?SERVER,{add_target_resource_type, Type}).

add_local_resource(Type, Resource) ->
    gen_server:cast(?SERVER, {add_local_resource, {Type, Resource}}).

fetch_resources(Type) ->
    gen_server:call(?SERVER, {fetch_resources, Type}).

trade_resources() ->
    gen_server:cast(?SERVER, trade_resources).

%% gen_server callbacks
init(_Args)  ->
    State = #state{target_resource_types = [],
                   local_resources = dict:new(),
                   found_resources = dict:new()},
    {ok, State}.

handle_cast({add_target_resource_type, Type},
            #state{target_resource_types=TargetTypes} = State) ->
    NewTargetTypes = [Type | lists:delete(Type, TargetTypes)],
    {noreply, State#state{target_resource_types=NewTargetTypes}};
handle_cast({add_local_resource, {Type, Resource}},
            #state{local_resources=LocalResources} = State) ->
    NewLocalResources = add_resource(Type, Resource, LocalResources),
    {noreply, State#state{local_resources=NewLocalResources}};
handle_cast(trade_resources, #state{local_resources=LocalResources} = State) ->
    AllNodes = [node() | nodes()],
    lists:foreach(
      fun(Node) ->
              gen_server:cast({?SERVER, Node},
                              {trade_resources, node(), LocalResources})
      end,
      AllNodes
     ),
    {noreply, State};
handle_cast({trade_resources, ReplyTo, Remotes},
            #state{target_resource_types = TargetTypes,
                   found_resources = OldFound,
                   local_resources = LocalResources} = State) ->
    FilteredRemotes = filter_remotes(TargetTypes, Remotes),
    NewFound = add_resources(FilteredRemotes, OldFound),
    case ReplyTo of
        noreply ->
            ok;
        _ ->
            gen_server:cast({?SERVER, ReplyTo},
                            {trade_resources, noreply, LocalResources})
    end,
    {noreply, State#state{found_resources=NewFound}};
handle_cast(stop, State) ->
    {stop, normal, State}.

handle_call({fetch_resources, Type}, _From,
            #state{found_resources=FoundResources} = State) ->
    {reply, dict:find(Type, FoundResources), State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVSN, State, _Extra) ->
    {ok, State}.

%% Util Functions
add_resource(Type, Resource, ResourceDict) ->
    case dict:find(Type, ResourceDict) of
        {ok, ResourceList} ->
            NewResourceList = [Resource | lists:delete(Resource, ResourceList)],
            dict:store(Type, NewResourceList, ResourceDict);
        error ->
            dict:store(Type, [Resource], ResourceDict)
    end.

filter_remotes(TargetTypes, ResourcesDict) ->
    Fun =
        fun(TargetType, Acc) ->
                case dict:find(TargetType, ResourcesDict) of
                    {ok, ResourceList} ->
                        [{TargetType, Resource} || Resource <- ResourceList] ++ Acc;
                     error ->
                        Acc
                end
        end,
    lists:foldl(Fun, [], TargetTypes).

add_resources([{Type, Resource} | Tail], ResourceDict) ->
    add_resources(Tail, add_resource(Type, Resource, ResourceDict));
add_resources([], ResourceDict) ->
    ResourceDict.
