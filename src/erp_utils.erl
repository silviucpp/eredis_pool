-module(erp_utils).

-export([
    get_env/1,
    get_env/2,
    get_priv_path/1,
    lookup/2,
    lookup/3,
    to_binary/1,
    join/2
]).

get_env(Key) ->
    get_env(Key, null).

get_env(Key, Default) ->
    case application:get_env(eredis_pool, Key) of
        {ok, Value} ->
            Value;
        _ ->
            Default
    end.

get_priv_path(File) ->
    case code:priv_dir(eredis_pool) of
        {error, bad_name} ->
            Ebin = filename:dirname(code:which(?MODULE)),
            filename:join([filename:dirname(Ebin), "priv", File]);
        Dir ->
            filename:join(Dir, File)
    end.

lookup(Key, List, Default) ->
    case lists:keyfind(Key, 1, List) of
        {Key, Result} ->
            Result;
        false ->
            Default
    end.

lookup(Key, List) ->
    lookup(Key, List, null).

to_binary(V) when is_binary(V) ->
    V;
to_binary(V) when is_atom(V) ->
    atom_to_binary(V, utf8);
to_binary(V) when is_list(V) ->
    iolist_to_binary(V);
to_binary(V) when is_integer(V) ->
    integer_to_binary(V);
to_binary(V) when is_float(V) ->
    float_to_binary(V, [{decimals, 8}, compact]).


join([Head | Tail], Sep) ->
    join_list_sep(Tail, Sep, [Head]);
join([], _Sep) ->
    <<>>.

join_list_sep([Head | Tail], Sep, Acc) ->
    join_list_sep(Tail, Sep, [Head, Sep | Acc]);
join_list_sep([], _Sep, Acc) ->
    list_to_binary(lists:reverse(Acc)).