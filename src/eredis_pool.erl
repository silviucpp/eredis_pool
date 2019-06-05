-module(eredis_pool).

-include("eredis_pool.hrl").

-export([
    start/0,
    start/1,
    stop/0,
    restart_pool/1,
    q/2
]).

-define(DEFAULT_TIMEOUT, 1000).

-spec start() ->
    ok | {error, reason()}.

start() ->
    start(temporary).

-spec start(permanent | transient | temporary) ->
    ok | {error, reason()}.

start(Type) ->
    case application:ensure_all_started(eredis_pool, Type) of
        {ok, _} ->
            ok;
        Other ->
            Other
    end.

-spec stop() ->
    ok.

stop() ->
    application:stop(eredis_pool).

-spec restart_pool(atom()) ->
    ok | {error, reason()}.

restart_pool(PoolName) ->
    case erp_cached_config:get_shards_map(PoolName) of
        {ok, ShardsMap} ->
            Nodes = lists:flatten(maps:values(ShardsMap)),
            lists:foreach(fun(NodeTag) -> true = erp_node_pool:restart(NodeTag) end, Nodes);
        Error ->
            Error
    end.

-spec q(atom(), [any()]) ->
    {ok, return_value()} | {error, redis_error()}.

q(PoolName, Command) ->
    case Command of
        [_, Key|_] ->
            run_command(PoolName, erp_utils:to_binary(Key), Command);
        _ ->
            % commands without a key are sent to a random node -> example: PING
            run_command(PoolName, <<"">>, Command)
    end.

% internals

run_command(PoolName, Key, Command) ->
    case erp_cached_config:get_shards_map(PoolName) of
        {ok, ShardsMap} ->
            case maps:size(ShardsMap) of
                0 ->
                    {error, no_nodes_available};
                ShardMapSize ->
                    SelectedShard = jchash:compute(erl_hash:murmur2(Key), ShardMapSize),
                    {ok, NodesTags} = maps:find(SelectedShard, ShardsMap),
                    do_run_command(NodesTags, Command)
            end;
        Error ->
            Error
    end.

do_run_command([NodeTag|RemainingNodes], Command) ->
    Result = erp_node_pool:q(NodeTag, Command, ?DEFAULT_TIMEOUT),
    case should_failover(Result) of
        false ->
            Result;
        _ ->
            ?ERROR_MSG("redis command: ~p on: ~p failed with: ~p -> will failover on: ~p", [Command, NodeTag, Result, RemainingNodes]),
            do_run_command(RemainingNodes, Command)
    end;
do_run_command([], _Command) ->
    {error, no_nodes_available}.

should_failover({ok, _}) ->
    false;
should_failover({error, Reason}) ->
    case Reason of
        {timeout, _} ->
            true;
        no_connection ->
            true;
        {tcp_error, _} ->
            true;
        {connection_error, _} ->
            true;
        _ ->
            false
    end;
should_failover(_) ->
    false.
