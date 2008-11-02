-module(wrsd_realm).

-export([process/1, record_to_json/1]).

-record(realm, {name, status, type, population}).
-include_lib("xmerl/include/xmerl.hrl").

process(Type) ->
    case [Type, fetch(Type)] of
        [us, {ok, Body}] -> decompose_body(us, Body);
        [eu, {ok, Body}] -> decompose_body(eu, Body);
	_ -> []
    end.

fetch(Type) ->
    Url = url(Type),
    case http:request(get, {Url, []}, [], []) of
        {ok, {_, _, Body}} -> {ok, Body};
        X -> {error, X}
    end.

url(us) -> "http://www.worldofwarcraft.com/realmstatus/status.xml";
url(eu) -> "http://www.wow-europe.com/realmstatus/index.html".

decompose_body(eu, Body) ->
    {match, Matches} = regexp:matches(Body, "<a name=\"([^\"]*)\"></a>"),
    lists:ukeysort(2, [begin
        RawMatch = string:substr(Body, Pos, Len),
	RealmName = string:sub_string(RawMatch, 10, length(RawMatch) - 6),
        #realm{ name = list_to_binary(RealmName), status = 99, type = 99, population = 99 }
    end || {Pos, Len} <- Matches]);

decompose_body(us, Body) ->
    try xmerl_scan:string(Body) of
        {XmlElem, _} ->
            %% {ok, XmlElem};
            [begin
                aggregate_realm(
                    [{Attr#xmlAttribute.name, Attr#xmlAttribute.value}
                        || Attr <- Elem#xmlElement.attributes],
                    #realm{}
                )
            end || Elem <- xmerl_xpath:string("//rs/r", XmlElem)];
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

record_to_json(Rec) ->
    Data = [
        {<<"name">>, Rec#realm.name},
        {<<"status">>, list_to_binary(clean_status(Rec#realm.status))},
        {<<"type">>, list_to_binary(clean_type(Rec#realm.type))},
        {<<"population">>, list_to_binary(clean_population(Rec#realm.population))}
    ],
    mochijson2:encode({struct, Data}).

clean_status(99) -> "unknown";
clean_status(1) -> "up";
clean_status(_) -> "down".

clean_type(99) -> "unknown";
clean_type(1) -> "pve";
clean_type(2) -> "pvp";
clean_type(3) -> "rp";
clean_type(_) -> "pvprp".

clean_population(99) -> "unknown";
clean_population(2) -> "medium";
clean_population(3) -> "high";
clean_population(4) -> "max";
clean_population(_) -> "low".

