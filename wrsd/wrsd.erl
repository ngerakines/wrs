-module(wrsd).
-behaviour(application).

-export([start/2, stop/1, init/1]).
-export([fetch_loop/1, update_loop/0, stop/0]).

start(_Type, _Args) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

stop(_State) -> ok.

stop() ->
    [Node | _] = init:get_plain_arguments(),
    Resp = rpc:call(list_to_atom(Node), init, stop, []),
    Resp.

init(Args) ->
    {ok, {{one_for_one, 2, 10}, [
        {wrsd_eurealmserver, {wrsd_eurealmserver, start_link, [Args]}, permanent, 2000, worker, [wrsd_eurealmserver]},
        {wrsd_usrealmserver, {wrsd_usrealmserver, start_link, [Args]}, permanent, 2000, worker, [wrsd_usrealmserver]},
        {wrsd_processor, {wrsd, fetch_loop, [none]}, permanent, 2000, worker, dynamic},
        {wrsd_web, {wrsd_web, start, [nil]}, permanent, 2000, worker, dynamic}
    ]}}.

fetch_loop(_) ->
    Pid = spawn_link(?MODULE, update_loop, []),
    {ok, Pid}.

update_loop() ->
    spawn(fun() -> [wrsd_usrealmserver:write(Realm) || Realm <- wrsd_realm:process(us)] end),
    spawn(fun() -> [wrsd_eurealmserver:write(Realm) || Realm <- wrsd_realm:process(eu)] end),
    timer:sleep(60000*10),
    update_loop().
