
-module(eredis_pool_app).

-behaviour(application).

-export([
    start/2,
    stop/1
]).

start(_StartType, _StartArgs) ->
    ok = erp_config_parser:create_pools(get_pools()),
    eredis_pool_sup:start_link().

stop(_State) ->
    erlpool:stop_group(eredis_pool),
    ok.

% internals

get_pools() ->
    case erp_utils:get_env(pools) of
        null ->
            [];
        Value ->
            Value
    end.
