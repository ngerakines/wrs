-module(wrsd_realmserver).
-behaviour(gen_server).

-export([start_link/1, init/1, handle_call/3, handle_cast/2, handle_info/2]).
-export([terminate/2, code_change/3, start/0, write/1, lookup/1, realms/0]).
-export([process/1, record_to_xml/1, fetch/1]).

-include_lib("xmerl/include/xmerl.hrl").

-record(realm, {name, status, type, population}).
-record(state, {realms = gb_trees:empty()}).

start_link(_) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_) -> {ok, #state{}}.

handle_call({write, Record}, _From, State) ->
    NewTree = gb_trees:enter(Record#realm.name, Record, State#state.realms),
    NewState = State#state{ realms = NewTree },
    {reply, ok, NewState};

handle_call({lookup, Key}, _From, State) ->
    LookupKey = case is_binary(Key) of false -> list_to_binary(Key); _ -> Key end,
    case gb_trees:lookup(LookupKey, State#state.realms) of
        {value, Rec} -> {reply, {ok, Rec}, State};
        _ -> {reply, {error, not_found}, State}
    end;

handle_call({realm_list}, _From, State) ->
    {reply, {ok, gb_trees:keys(State#state.realms)}, State};

handle_call(Request, _From, State) -> {stop, {unknown_call, Request}, State}.

handle_cast(_Message, State) -> {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> application:stop(yaws), ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

start() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

write(Rec) when is_record(Rec, realm) ->
    gen_server:call(?MODULE, {write, Rec}).

lookup(Realm) ->
    gen_server:call(?MODULE, {lookup, Realm}).

realms() ->
    gen_server:call(?MODULE, {realm_list}).

process(Type) ->
    case fetch(Type) of
        {ok, Body} ->
            decompose_body(Body);
        _ -> []
    end.

fetch(Type) ->
    case http:request(get, {url(Type), []}, [], []) of
        {ok, {_, _, Body}} -> {ok, Body};
        X -> {error, X}
    end.

url(_) -> "http://www.worldofwarcraft.com/realmstatus/status.xml".

decompose_body(Body) ->
    try xmerl_scan:string(Body) of
        {XmlElem, _} ->
            [begin
                aggregate_realm(
                [{Attr#xmlAttribute.name, Attr#xmlAttribute.value} || Attr <- Elem#xmlElement.attributes],
                #realm{}
            ) end || Elem <- xmerl_xpath:string("/page/rs/r", XmlElem)];
        _ -> {error, unknown}
    catch
        _:_ -> {error, throw}
    end.

aggregate_realm([], Rec) -> Rec;
aggregate_realm([Head | Tail], Rec) ->
    NewRec = case Head of
        {n, Name} -> Rec#realm{ name = list_to_binary(Name) };
        {s, Status} -> Rec#realm{ status = list_to_integer(Status) };
        {t, Type} -> Rec#realm{ type = list_to_integer(Type) };
        {l, Population} -> Rec#realm{ population = list_to_integer(Population) }
    end,
    aggregate_realm(Tail, NewRec).

record_to_xml(Rec) ->
    Data = [
        {name, [], [binary_to_list(Rec#realm.name)]},
        {status, [], [clean_status(Rec#realm.status)]},
        {type, [], [clean_type(Rec#realm.type)]},
        {population, [], [clean_population(Rec#realm.population)]}
    ],
    {RootEl, _} = xmerl_scan:string("<realm xmlns=\"urn:wrsd:realm\" />"),
    #xmlElement{content = Content} = RootEl,
    NewContent = Content ++ lists:flatten([Data]),
    NewRootEl=RootEl#xmlElement{content=NewContent},    
    Export=xmerl:export_simple([NewRootEl], xmerl_xml),
    lists:flatten(Export).

clean_status(1) -> "up";
clean_status(_) -> "down".

clean_type(1) -> "pve";
clean_type(2) -> "pvp";
clean_type(3) -> "rp";
clean_type(_) -> "pvprp".

clean_population(2) -> "medium";
clean_population(3) -> "high";
clean_population(4) -> "max";
clean_population(_) -> "low".
