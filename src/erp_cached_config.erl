-module(erp_cached_config).

%% Module stub.
%% Will be replaced by the module with the same name at runtime.
%% The only purpose for the module is to suppress warnings from code analyzers,
%% as dynamically compiled module is not available during the build.

-export([
    get_shards_map/1
]).

get_shards_map(_) ->
    {error, not_found}.
