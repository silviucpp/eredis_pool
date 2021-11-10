-module(erp_config_parser).

-include("eredis_pool.hrl").

-define(CACHED_CONFIG_MODULE, erp_cached_config).

-export([
    create_pools/1
]).

create_pools(Pools) ->
    Fun = fun({PoolName, PoolArgs}, Acc) ->
        PoolNameBin = atom_to_binary(PoolName, latin1),
        Shards = get_shards(erp_utils:lookup(shards, PoolArgs, []), PoolNameBin, []),
        ConnectionsPerHost = erp_utils:lookup(connections_per_host, PoolArgs, 3),
        ConnectionOpts = erp_utils:lookup(connection_options, PoolArgs, []),

        % create pool for each node in each shard

        lists:foreach(fun({Tag, Host, Port}) ->
            ok = erp_node_pool:start(Tag, Host, Port, ConnectionsPerHost, ConnectionOpts)
        end, lists:flatten(Shards)),

        [{PoolName, Shards} | Acc]
    end,
    Specs = lists:foldl(Fun, [], Pools),
    ok = compile_pool_specs(Specs).

% internals

compile_pool_specs(Specs) ->
    code:purge(?CACHED_CONFIG_MODULE),
    case dynamic_compile:load_from_string(get_pool_specs_code(Specs)) of
        {module, ?CACHED_CONFIG_MODULE} ->
            ok;
        Error ->
            Error
    end.

get_pool_specs_code(SettingsList) ->
    binary_to_list(get_pool_specs_code(SettingsList, <<>>)).

get_pool_specs_code([{PoolName, Shards}|T], AccBody) ->
    PoolNameBin = atom_to_binary(PoolName, latin1),
    ShardsMapBin = list_to_binary(io_lib:format("~tp", [shards_to_map(Shards, 0, [])])),
    ModuleFun = <<"get_shards_map('", PoolNameBin/binary,"') -> {ok, ", ShardsMapBin/binary,"};\n">>,
    get_pool_specs_code(T, <<AccBody/binary, ModuleFun/binary>>);
get_pool_specs_code([], AccBody0) ->
    AccBody = <<AccBody0/binary, "get_shards_map(_) -> {error, not_found}.\n">>,
    ModuleBin = atom_to_binary(?CACHED_CONFIG_MODULE, latin1),
    ModuleHeader = <<"-module(", ModuleBin/binary, ").\n -export([get_shards_map/1]).\n">>,
    <<ModuleHeader/binary, AccBody/binary>>.

shards_to_map([Nodes0|T], Index, Acc) ->
    Nodes = lists:map(fun({Tag, _Host, _Port}) -> Tag end, Nodes0),
    shards_to_map(T, Index+1, [{Index, Nodes} |Acc]);
shards_to_map([], _Index, Acc) ->
    maps:from_list(Acc).

get_shards([H|T], PoolNameBin, Acc) ->
    Nodes = lists:map(fun(X) -> to_node(X, PoolNameBin) end, binary:split(H, <<",">>, [global])),
    get_shards(T, PoolNameBin, [Nodes|Acc]);
get_shards([], _PoolNameBin, Acc) ->
    lists:reverse(Acc).

to_node(S, PoolNameBin) ->
    [Host, Port] = binary:split(S, <<":">>),
    Tag = binary_to_atom(<<"node_", PoolNameBin/binary, "_", Host/binary, "_", Port/binary>> , utf8),
    {Tag, binary_to_list(Host), binary_to_integer(Port)}.

