%%==============================================================================
%% Copyright 2015 Erlang Solutions Ltd.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%==============================================================================
-module(mongoose_riak).

-include("ejabberd.hrl").
-include_lib("riakc/include/riakc.hrl").

%% API
-export([start/0]).
-export([stop/0]).

-export([start_worker/2, start_worker/3]).

-export([put/1, put/2]).
-export([get/2, get/3]).
-export([delete/2, delete/3]).
-export([update_type/3, update_type/4]).
-export([fetch_type/2, fetch_type/3]).
-export([list_keys/1]).
-export([get_worker/0]).
-export([create_new_map/1]).
-export([update_map/2]).

-export([make_pool_name/1, get_pool_name/1]).

-compile({no_auto_import,[put/2]}).

-define(CALL(F, Args), call_riak(F, Args)).
-define(POOL_NAME(Id), "riak_pool_" ++ integer_to_list(Id)).

-type riakc_map_op() :: {{binary(), riakc_map:datatype()},
                          fun((riakc_datatype:datatype()) -> riakc_datatype:datatype())}.

-spec start() -> {ok, pid()} | ignore.
start() ->
    case ejabberd_config:get_local_option({riak_config, ?MYNAME}) of
        undefined ->
            ignore;
        RiakOpts ->
            {_, Workers} = lists:keyfind(workers, 1, RiakOpts),
            {_, PoolsSpec} = lists:keyfind(pools_spec, 1, RiakOpts),
            mongoose_riak_sup:start(Workers, PoolsSpec)
    end.
-spec stop() -> no_return().
stop() ->
    mongoose_riak_sup:stop().

-spec start_worker(riakc_pb_socket:address(), riakc_pb_socket:portnum())
        -> {ok, pid()} | {error, term()}.
start_worker(Address, Port) ->
    start_worker(Address, Port, []).

-spec start_worker(riakc_pb_socket:address(), riakc_pb_socket:portnum(),
                   proplists:proplist())
        -> {ok, pid()} | {error, term()}.
start_worker(Address, Port, Opts) ->
    riakc_pb_socket:start_link(Address, Port, Opts).

-spec put(riakc_obj()) ->
    ok | {ok, riakc_obj()} | {ok, key()} | {error, term()}.
put(Obj) ->
    put(Obj, []).

-spec put(riakc_obj(), timeout() | put_options()) ->
    ok | {ok, riakc_obj()} | {ok, key()} | {error, term()}.
put(Obj, OptsOrTimeout) ->
    ?CALL(put, [Obj, OptsOrTimeout]).

-spec get(bucket(), key()) -> {ok, riakc_obj()} | {error, term()}.
get(Bucket, Key) ->
    get(Bucket, Key, []).

-spec get(bucket(), key(), get_options() | timeout()) ->
    {ok, riakc_obj()} | {error, term()}.
get(Bucket, Key, OptsOrTimeout) ->
    ?CALL(get, [Bucket, Key, OptsOrTimeout]).


-spec update_type({binary(), binary()}, binary(), riakc_datatype:update(term())) ->
    ok.
update_type(Bucket, Key, Update) ->
    update_type(Bucket, Key, Update, []).

-spec update_type({binary(), binary()}, binary(),
    riakc_datatype:update(term()), [proplists:property()]) ->
    ok.
update_type(Bucket, Key, Update, Options) ->
    ?CALL(update_type, [Bucket, Key, Update, Options]).

-spec delete(bucket(), key()) ->
    ok | {error, term()}.
delete(Bucket, Key) ->
    delete(Bucket, Key, []).

-spec delete(bucket(), key(), delete_options() | timeout()) ->
    ok | {error, term()}.
delete(Bucket, Key, OptsOrTimeout) ->
    ?CALL(delete, [Bucket, Key, OptsOrTimeout]).

-spec fetch_type({binary(), binary()}, binary()) ->
    {ok, riakc_datatype:datatype()} | {error, term()}.
fetch_type(Bucket, Key) ->
    fetch_type(Bucket, Key, []).

-spec fetch_type({binary(), binary()}, binary(), [proplists:property()]) ->
    {ok, riakc_datatype:datatype()} | {error, term()}.
fetch_type(Bucket, Key, Opts) ->
    ?CALL(fetch_type, [Bucket, Key, Opts]).

-spec list_keys({binary(), binary()}) ->
    {ok, [binary()]} | {error, term()}.
list_keys(Bucket) ->
    ?CALL(list_keys, [Bucket]).


-spec create_new_map([riakc_map_op()]) -> riakc_map:crdt_map().
create_new_map(Ops) ->
    update_map(riakc_map:new(), Ops).

-spec update_map(riakc_map:crdt_map(), [riakc_map_op()]) -> riakc_map:crdt_map().
update_map(Map, Ops) ->
    lists:foldl(fun update_map_op/2, Map, Ops).

-spec get_worker() -> pid() | undefined.
get_worker() ->
    Pool = pick_pool(mongoose_riak_sup:get_riak_pools_count()),
    case catch cuesport:get_worker(Pool) of
        Pid  when is_pid(Pid) ->
            Pid;
        _ ->
            undefined
    end.
-spec make_pool_name(integer()) -> atom().
make_pool_name(Id) ->
    list_to_atom(?POOL_NAME(Id)).
-spec get_pool_name(integer()) -> atom().
get_pool_name(Id) ->
    list_to_existing_atom(?POOL_NAME(Id)).

update_map_op({Field, Fun}, Map) ->
    riakc_map:update(Field, Fun, Map).

call_riak(F, ArgsIn) ->
    Worker = get_worker(),
    Args = [Worker | ArgsIn],
    apply(riakc_pb_socket, F, Args).

pick_pool(undefined) ->
    undefined;
pick_pool(1) -> %% no need to draw a pool as there's only one
    riak_pool_1;
pick_pool(Count) ->
    PoolId = erlang:phash2({os:timestamp(), self()}, Count) + 1,
    get_pool_name(PoolId).