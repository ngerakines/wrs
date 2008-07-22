-module(wrsd).
-behaviour(application).

-export([start/2, stop/1]).
-compile(export_all).

-include("wrsd.hrl").

start(_Type, Args) -> wrsd_sup:start_link(Args).

stop(_State) -> ok.

start_phase(update_loop, _, _) ->
    spawn(?MODULE, update_loop, []),
    ok.

update_loop() ->
    spawn(fun() -> [wrsd_usrealmserver:write(Realm) || Realm <- wrsd_realm:process(us)] end),
    spawn(fun() -> [wrsd_eurealmserver:write(Realm) || Realm <- wrsd_realm:process(eu)] end),
    timer:sleep(?UPDATEINTERVAL),
    wrsd:update_loop().

