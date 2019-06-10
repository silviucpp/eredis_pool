-module(erp_node_pool).

-include("eredis_pool.hrl").

-export([
    start/5,
    stop/1,
    restart/1,
    q/3,
    qp/3,
    transaction/3
]).

start(NodeTag, Host, Port, ConnectionsPerHost, ConnectionOpts) ->
    ?INFO_MSG("start pool with tag: ~p host: ~p port: ~p connections: ~p", [NodeTag, Host, Port, ConnectionsPerHost]),

    ok = erlpool:start_pool(NodeTag, [
        {size, ConnectionsPerHost},
        {group, eredis_pool},
        {start_mfa, {eredis, start_link, [[{host, Host}, {port, Port} | ConnectionOpts]]}}
    ]).

stop(NodeTag) ->
    ?INFO_MSG("stop pool with tag: ~p", [NodeTag]),
    erlpool:stop_pool(NodeTag).

restart(NodeTag) ->
    ?INFO_MSG("restart pool with tag: ~p", [NodeTag]),
    erlpool:restart_pool(NodeTag).

q(NodeTag, Command, Timeout) ->
    try
        eredis:q(erlpool:pid(NodeTag), Command, Timeout)
    catch _:Error ->
        handle_exception(Error, Command)
    end.

qp(NodeTag, Pipeline, Timeout) ->
    try
        eredis:qp(erlpool:pid(NodeTag), Pipeline, Timeout)
    catch _:Error ->
        handle_exception(Error, Pipeline)
    end.

transaction(NodeTag, Commands0, Timeout) ->
    case qp(NodeTag, [[<<"MULTI">>] | Commands0] ++ [[<<"EXEC">>]], Timeout) of
        Rs when is_list(Rs) ->
            lists:last(Rs);
        Other ->
            Other
    end.

% internals

handle_exception({Error, _}, Command) ->
    {error, {Error, Command}};
handle_exception(Error, Command) ->
    {error, {Error, Command}}.
