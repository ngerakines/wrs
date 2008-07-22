-module(wrsd_sup).
-behaviour(supervisor).

-export([start_link/1, init/1]).

start_link(_) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(Args) ->
    {ok, {{one_for_one, 2, 10}, [
        {wrsd_usrealmserver, {wrsd_usrealmserver, start_link, [Args]}, permanent, 2000, worker, [wrsd_usrealmserver]},
        {wrsd_eurealmserver, {wrsd_eurealmserver, start_link, [Args]}, permanent, 2000, worker, [wrsd_eurealmserver]},
        {wrsd_yaws, {wrsd_yaws, start_link, [Args]}, permanent, 2000, worker, [wrsd_yaws]}
    ]}}.

