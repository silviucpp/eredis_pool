-module(eredis_pool).

-include("eredis_pool.hrl").

-define(OP_QUERY, query).
-define(OP_PIPELINE, pipeline).
-define(OP_TRANSACTION, transaction).

-export([
    start/0,
    start/1,
    stop/0,
    restart_pool/1,
    q/2,
    qp/2,
    transaction/2
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
    run_command(PoolName, get_key(Command), ?OP_QUERY, Command).

-spec qp(atom(), pipeline()) ->
    [{ok, return_value()} | {error, redis_error()}] | {error, no_nodes_available}.

qp(PoolName, Pipeline) ->
    run_command(PoolName, get_pipeline_key(Pipeline), ?OP_PIPELINE, Pipeline).

-spec transaction(atom(), pipeline()) ->
    {ok, [return_value()]} | {error, redis_error()}.

transaction(PoolName, Pipeline) ->
    run_command(PoolName, get_pipeline_key(Pipeline), ?OP_TRANSACTION, Pipeline).

% internals

run_command(PoolName, Key, CommandType, Command) ->
    case erp_cached_config:get_shards_map(PoolName) of
        {ok, ShardsMap} ->
            case maps:size(ShardsMap) of
                0 ->
                    {error, no_nodes_available};
                ShardMapSize ->
                    Hash = erl_hash:murmur2(Key),
                    {ok, NodesTags} = maps:find(jchash:compute(Hash, ShardMapSize), ShardsMap),

                    case length(NodesTags) of
                        NodesTagLength when NodesTagLength > 1 ->
                            do_run_command(shuffle_list(NodesTags, 0, Hash rem NodesTagLength, []), CommandType, Command);
                        _ ->
                            do_run_command(NodesTags, CommandType, Command)
                    end
            end;
        Error ->
            Error
    end.

do_run_command([NodeTag|RemainingNodes], CommandType, Command) ->
    Result = exec(CommandType, NodeTag, Command),
    case should_failover(Result) of
        false ->
            Result;
        _ ->
            ?LOG_ERROR("redis command: ~p on: ~p failed with: ~p -> will failover on: ~p", [Command, NodeTag, Result, RemainingNodes]),
            do_run_command(RemainingNodes, CommandType, Command)
    end;
do_run_command([], _CommandType,  _Command) ->
    {error, no_nodes_available}.

exec(?OP_QUERY, NodeTag, Command) ->
    erp_node_pool:q(NodeTag, Command, ?DEFAULT_TIMEOUT);
exec(?OP_PIPELINE, NodeTag, Command) ->
    erp_node_pool:qp(NodeTag, Command, ?DEFAULT_TIMEOUT);
exec(?OP_TRANSACTION, NodeTag, Command) ->
    erp_node_pool:transaction(NodeTag, Command, ?DEFAULT_TIMEOUT).

get_key([RedisCommand, Key | Other])  ->
    case is_eval_or_evalsha(RedisCommand) of
        false ->
            erp_utils:to_binary(Key);
        {true, Type} ->
            case Other of
                [KeysCount, FirstKey| _OtherKeysAndArgs] when KeysCount > 0 ->
                    erp_utils:to_binary(FirstKey);
                _ ->
                    case Type of
                        evalsha ->
                            Key;
                        _ ->
                            erp_utils:sha_hex(Key)
                    end
            end
    end;
get_key(_) ->
    % just send the request to a random node
    <<"">>.

get_pipeline_key([H|T]) ->
    case get_key(H) of
        K when byte_size(K) > 0 ->
            K;
        _ ->
            get_pipeline_key(T)
    end;
get_pipeline_key([]) ->
    <<>>.

shuffle_list([H|T], Index, DesiredIndex, Acc) ->
    case Index of
        DesiredIndex ->
            [H | Acc] ++ T;
        _ ->
            shuffle_list(T, Index+1, DesiredIndex, [H|Acc])
    end.

should_failover({ok, _}) ->
    false;
should_failover(L) when is_list(L) ->
    % match pipeline responses
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

is_eval_or_evalsha(C) when C == <<"EVALSHA">> orelse C == <<"evalsha">> ->
    {true, evalsha};
is_eval_or_evalsha(C) when C == <<"EVAL">> orelse C == <<"eval">> ->
    {true, eval};
is_eval_or_evalsha(_) ->
    false.
