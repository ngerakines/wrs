-module(wrsd_yaws).
-behaviour(gen_server).

-include("wrsd.hrl").
-include_lib("yaws/include/yaws.hrl").

-export([start_link/1, init/1, handle_call/3, handle_cast/2, handle_info/2]).
-export([terminate/2, code_change/3, set_conf/0]).

start_link(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

init(_) ->
    process_flag(trap_exit, true),
    wrsd_yaws:set_conf().

set_conf() ->
    GC = yaws_config:make_default_gconf(false, "wrsd"),
    SC1 = #sconf{
        port = ?PORT,
        servername = ?PRODDOMAIN,
        listen = ?PRODIP,
        docroot = "www",
        appmods = [{"/", wrsd_handler}]
    },
    SC2 = #sconf{
        port = ?PORT,
        servername = ?DEVDOMAIN,
        listen = ?DEVIP,
        docroot = "www",
        appmods = [{"/", wrsd_handler}]
    },
    try yaws_api:setconf(GC, [[SC1, SC2]]) of
        ok -> {ok, started};
        Errora -> {stop, Errora}
    catch
        Errorb -> {stop, Errorb}
    end.

handle_call(Request, _From, State) -> {stop, {unknown_call, Request}, State}.

handle_cast(_Message, State) -> {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> application:stop(yaws), ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
