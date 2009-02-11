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
-module(wrsd_eurealmserver).
-behaviour(gen_server).

-export([start_link/1, init/1, handle_call/3, handle_cast/2, handle_info/2]).
-export([terminate/2, code_change/3, write/1, lookup/1, realms/0]).

-record(realm, {name, status, type, population}).
-record(state, {realms = gb_trees:empty()}).

start_link(_) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_) -> {ok, #state{}}.

handle_call({write, Record}, _From, State) ->
    NewTree = gb_trees:enter(Record#realm.name, Record, State#state.realms),
    NewState = State#state{ realms = NewTree },
    {reply, ok, NewState};

handle_call({lookup, Key}, _From, State) ->
    LookupKey = case is_binary(Key) of false -> list_to_binary(Key); _ -> Key end,
    case gb_trees:lookup(LookupKey, State#state.realms) of
        {value, Rec} -> {reply, {ok, Rec}, State};
        _ -> {reply, {error, not_found}, State}
    end;

handle_call({realm_list}, _From, State) ->
    {reply, {ok, gb_trees:keys(State#state.realms)}, State};

handle_call(Request, _From, State) -> {stop, {unknown_call, Request}, State}.

handle_cast(_Message, State) -> {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> application:stop(yaws), ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

write(Rec) when is_record(Rec, realm) ->
    gen_server:call(?MODULE, {write, Rec}).

lookup(Realm) ->
    gen_server:call(?MODULE, {lookup, Realm}).

realms() ->
    gen_server:call(?MODULE, {realm_list}).

