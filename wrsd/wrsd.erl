%% Copyright (c) 2008 Nick Gerakines <nick@gerakines.net>
%% 
%% Permission is hereby granted, free of charge, to any person
%% obtaining a copy of this software and associated documentation
%% files (the "Software"), to deal in the Software without
%% restriction, including without limitation the rights to use,
%% copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the
%% Software is furnished to do so, subject to the following
%% conditions:
%% 
%% The above copyright notice and this permission notice shall be
%% included in all copies or substantial portions of the Software.
%% 
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
%% OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
%% NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
%% HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
%% WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
%% OTHER DEALINGS IN THE SOFTWARE.
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
