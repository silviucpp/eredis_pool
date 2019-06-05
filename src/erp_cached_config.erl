-module(erp_cached_config).

%% Module stub.
%% Will be replaced by the module with the same name at runtime.
%% The only purpose for the module is to suppress warnings from code analyzers,
%% as dynamically compiled module is not available during the build.

-export([
    get_shards_map/1
]).

-ifdef(common_test).
-define(DO_NOT_LOAD, ok).
-else.
-define(DO_NOT_LOAD, do_not_load).
-endif.

-on_load(do_not_load/0).

do_not_load() ->
    ?DO_NOT_LOAD.

get_shards_map(_) ->
    {error, not_found}.
