-module(mongoose_wpool_elastic).
-behaviour(mongoose_wpool).

-export([init/0]).
-export([start/4]).
-export([stop/2]).

init() ->
    tirerl:start(),
    ok.

start(Host, Tag, _WpoolOptsIn, ConnOpts) ->
    ElasticHost = proplists:get_value(host, ConnOpts, "localhost"),
    Port = proplists:get_value(port, ConnOpts, 9200),
    PoolName = mongoose_wpool:make_pool_name(elastic, Host, Tag),
    case tirerl:start_pool(PoolName, [{host, list_to_binary(ElasticHost)}, {port, Port}]) of
        {ok, Pid} ->
            {external, Pid};
        {ok, Pid, _} ->
            {external, Pid};
        Other ->
            Other
    end.

stop(Host, Tag) ->
    PoolName = mongoose_wpool:make_pool_name(elastic, Host, Tag),
    tirerl:stop_pool(PoolName),
    ok.

