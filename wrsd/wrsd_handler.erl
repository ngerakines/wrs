-module(wrsd_handler).

-export([out/1, handle_request/3]).

-include_lib("yaws/include/yaws_api.hrl").
-include_lib("xmerl/include/xmerl.hrl").
-include("wrsd.hrl").

out(Arg) ->
    Req = Arg#arg.req,
    {_, ReqPath} = Req#http_request.path,
    wrsd_handler:handle_request(Req#http_request.method, ReqPath, Arg).

handle_request('GET', "/realm/us/", _Arg) ->
    case gen_server:call(wrsd_usrealmserver, {realm_list}) of
        {ok, Realms} ->
	    XmlBody = rfc4627:encode(Realms),
            make_response(200, XmlBody);
        _ -> make_response(404, "<error>No data for that realm.</error>")
    end;

handle_request('GET', "/realm/eu/", _Arg) ->
    case gen_server:call(wrsd_eurealmserver, {realm_list}) of
        {ok, Realms} ->
	    XmlBody = rfc4627:encode(Realms),
            make_response(200, XmlBody);
        _ -> make_response(404, "<error>No data for that realm.</error>")
    end;

handle_request('GET', "/realm/us/" ++ Realm, _Arg) ->
    case gen_server:call(wrsd_usrealmserver, {lookup, Realm}) of
        {ok, Record} ->
            XmlBody = wrsd_realm:record_to_json(Record),
            make_response(200, XmlBody);
        _ -> make_response(404, "<error>No data for that realm.</error>")
    end;

handle_request('GET', "/realm/eu/" ++ Realm, _Arg) ->
    case gen_server:call(wrsd_eurealmserver, {lookup, Realm}) of
        {ok, Record} ->
            XmlBody = wrsd_realm:record_to_json(Record),
            make_response(200, XmlBody);
        _ -> make_response(404, "<error>No data for that realm.</error>")
    end;

handle_request('GET', "/realm/" ++ Realm, _Arg) ->
    case gen_server:call(wrsd_usrealmserver, {lookup, Realm}) of
        {ok, Record} ->
            XmlBody = wrsd_realm:record_to_json(Record),
            make_response(200, XmlBody);
        _ -> make_response(404, "<error>No data for that realm.</error>")
    end;

handle_request(_, _, _Arg) -> % catchall
    make_response(501, "<error>Action not implemented.</error>").

make_response(Status, Message) ->
    make_response(Status, "application/xml", Message).

make_response(Status, Type, Message) ->
    make_all_response(Status, make_header(Type), Message).

make_header(Type) -> [{header, ["Content-Type: ", Type]}].

make_all_response(Status, Headers, Message) ->
    [{status, Status}, {allheaders, Headers}, {html, Message}].
