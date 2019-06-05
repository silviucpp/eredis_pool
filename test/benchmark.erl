-module(benchmark).

-define(POOLNAME, pool1).

-export([
    bench/2
]).

get_key(N) ->
    <<"bench_key", (integer_to_binary(N))/binary>>.

bench(Number, Concurrency) ->
    eredis_pool:start(),
    List = lists:seq(1, Concurrency),

    Keys = lists:map(fun(X) -> get_key(X) end, List),
    ok = lists:foreach(fun(K) -> {ok, <<"OK">>} = eredis_pool:q(?POOLNAME, [<<"SET">>, K, K]) end, Keys),

    Self = self(),
    LoopNumbers = Number div Concurrency,

    io:format("### run test ->  concurrent clients: ~p requests per client: ~p ~n", [Concurrency, LoopNumbers]),

    A = os:timestamp(),
    Pids = [spawn_link(fun() -> loop(LoopNumbers, get_key(X)), Self ! {self(), done} end) || X <- List],
    [receive {Pid, done} -> ok end || Pid <- Pids],
    B = os:timestamp(),
    print(Number, A, B).

print(Num, A, B) ->
    Microsecs = timer:now_diff(B, A),
    Time = Microsecs div Num,
    PerSec = case Time of
         0 ->
             "N/A";
         _ ->
             1000000 div Time
    end,
    io:format("### ~p ms ~p req/sec ~n", [Microsecs div 1000, PerSec]).

loop(0, _Key) ->
    ok;
loop(Nr, Key) ->
    {ok, Key} = eredis_pool:q(?POOLNAME, [<<"GET">>, Key]),
    loop(Nr-1, Key).
