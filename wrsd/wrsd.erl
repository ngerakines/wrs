-module(wrsd).
-behaviour(application).

-export([start/2, stop/1, start_phase/3, init/1]).
-export([update_loop/0, build_rel/0]).

start(_Type, _Args) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


stop(_State) -> ok.

start_phase(update_loop, _, _) ->
    spawn(?MODULE, update_loop, []),
    ok.

init(Args) ->
    {ok, {{one_for_one, 2, 10}, [
        {wrsd_eurealmserver, {wrsd_eurealmserver, start_link, [Args]}, permanent, 2000, worker, [wrsd_eurealmserver]},
        {wrsd_usrealmserver, {wrsd_usrealmserver, start_link, [Args]}, permanent, 2000, worker, [wrsd_usrealmserver]},
        {wrsd_web, {wrsd_web, start, [nil]}, permanent, 2000, worker, dynamic}
    ]}}.

update_loop() ->
    spawn(fun() -> [wrsd_usrealmserver:write(Realm) || Realm <- wrsd_realm:process(us)] end),
    spawn(fun() -> [wrsd_eurealmserver:write(Realm) || Realm <- wrsd_realm:process(eu)] end),
    timer:sleep(60000*10),
    wrsd:update_loop().

%% @spec build_rel() -> ok
%% @doc Creates a .rel script for the project.
build_rel() ->
    {ok, FD} = file:open("wrsd.rel", [write]),
    RootDir = code:root_dir(),
    Patterns = [
        {RootDir ++ "/", "erts-*"},
        {RootDir ++ "/lib/", "kernel-*"},
        {RootDir ++ "/lib/", "stdlib-*"},
        {RootDir ++ "/lib/", "sasl-*"},
        {RootDir ++ "/lib/", "inets-*"}
    ],
    [Erts, Kerne, Stdl, Sasl, Inet] = [begin
        [R | _ ] = filelib:wildcard(P, D),
        [_ | [Ra] ] = string:tokens(R, "-"),
        Ra
    end || {D, P} <- Patterns],
    RelInfo = {release,
        {"wrsd", "0.2"},
        {erts, Erts}, [
            {kernel, Kerne},
            {stdlib, Stdl},
            {sasl, Sasl},
            {inets, Inet},
            {wrsd, "0.2"}
        ]
    },
    io:format(FD, "~p.", [RelInfo]),
    file:close(FD),
    systools:make_script("wrsd", [local]),
    ok.
