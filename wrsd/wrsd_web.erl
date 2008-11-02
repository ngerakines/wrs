-module(wrsd_web).
-export([start/1, stop/0, handle_request/3]).

start(_) ->
    Dispatcher = fun(Req) ->
        ?MODULE:handle_request(Req:get(method), Req:get(path), Req)
    end,
    Options = [
        {ip, "0.0.0.0"},
        {port, 5040}
    ],
    mochiweb_http:start([{name, ?MODULE}, {loop, Dispatcher} | Options]).

stop() ->
    mochiweb_http:stop(?MODULE).

handle_request('GET', "/realms/us/", Req) ->
    case wrsd_usrealmserver:realms() of
        {ok, Realms} ->
	        XmlBody = mochijson2:encode(Realms),
            make_response(Req, 200, XmlBody);
        _ -> make_response(Req, 404, "<error>No data for that realm.</error>")
    end;

handle_request('GET', "/realms/eu/", Req) ->
    case wrsd_eurealmserver:realms() of
        {ok, Realms} ->
	        XmlBody = mochijson2:encode(Realms),
            make_response(Req, 200, XmlBody);
        _ -> make_response(Req, 404, "<error>No data for that realm.</error>")
    end;

handle_request('GET', "/realm/us/" ++ Realm, Req) ->
    case wrsd_usrealmserver:lookup(Realm) of
        {ok, Record} ->
            XmlBody = wrsd_realm:record_to_json(Record),
            make_response(Req, 200, XmlBody);
        _ -> make_response(Req, 404, "<error>No data for that realm.</error>")
    end;

handle_request('GET', "/realm/eu/" ++ Realm, Req) ->
    case wrsd_eurealmserver:lookup(Realm) of
        {ok, Record} ->
            XmlBody = wrsd_realm:record_to_json(Record),
            make_response(Req, 200, XmlBody);
        _ -> make_response(Req, 404, "<error>No data for that realm.</error>")
    end;

handle_request('GET', "/realm/" ++ Realm, Req) ->
    case wrsd_usrealmserver:lookup(Realm) of
        {ok, Record} ->
            XmlBody = wrsd_realm:record_to_json(Record),
            make_response(Req, 200, XmlBody);
        _ -> make_response(Req, 404, "<error>No data for that realm.</error>")
    end;

handle_request(_, _, Req) ->
    make_response(Req, 501, "<error>Action not implemented.</error>").

make_response(Req, Status, Body) ->
    Req:respond({
        Status,
        [{"Content-Type", "application/xml"}],
        Body
    }).
