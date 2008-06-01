-module(wrsd).
-behaviour(application).

-compile(export_all).

start(_Type, Args) -> wrsd_sup:start_link(Args).

stop(_State) -> ok.

start_phase(update_loop, _, _) ->
    spawn(?MODULE, update_loop, []),
    ok.

update_loop() ->
    spawn(fun() -> [wrsd_realmserver:write(Realm) || Realm <- wrsd_realm:process(us)] end),
    timer:sleep(?UPDATEINTERVAL),
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
        {RootDir ++ "/lib/", "inets-*"},
    ],
    [Erts, Kerne, Stdl, Sasl, Inet] = [begin
        [R | _ ] = filelib:wildcard(P, D),
        [_ | [Ra] ] = string:tokens(R, "-"),
        Ra
    end || {D, P} <- Patterns],
    RelInfo = {release,
        {"wrsd", wrsd:version()},
        {erts, Erts}, [
            {kernel, Kerne},
            {stdlib, Stdl},
            {sasl, Sasl},
            {inets, Inet},
            {wrsd, wrsd:version()}
        ]
    },
    io:format(FD, "~p.", [RelInfo]),
    file:close(FD),
    systools:make_script("wrsd", [local]),
    ok.
