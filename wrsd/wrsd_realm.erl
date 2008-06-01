-module(wrsd_realm).

-export([process/1, record_to_xml/1]).

-include_lib("xmerl/include/xmerl.hrl").
-include("wrsd.hrl").

process(Type) ->
    case fetch(Type) of
        {ok, Body} -> decompose_body(Body);
        _ -> []
    end.

fetch(Type) ->
    Url = url(Type),
    case http:request(get, {Url, []}, [], []) of
        {ok, {_, _, Body}} -> {ok, Body};
        X -> {error, X}
    end.

url(_) -> "http://www.worldofwarcraft.com/realmstatus/status.xml".

decompose_body(Body) ->
    try xmerl_scan:string(Body) of
        {XmlElem, _} ->
            %% {ok, XmlElem};
            [begin
                aggregate_realm(
                    [{Attr#xmlAttribute.name, Attr#xmlAttribute.value}
                        || Attr <- Elem#xmlElement.attributes],
                    #realm{}
                )
            end || Elem <- xmerl_xpath:string("/rs/r", XmlElem)];
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
