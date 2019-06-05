eredis_pool
=======

[![Build Status](https://travis-ci.org/silviucpp/eredis_pool.svg?branch=master)](https://travis-ci.org/silviucpp/eredis_pool)

An Erlang pool for Redis with consistent hashing based on [erlpool][2] and [eredis][3]. 
This project was developed to be used with [KeyDB][1] by using Active Replication and Multi-Master features, but can be used as well with Redis.  

Features
---------

- Connection pool with persistent connections - this helps reduce connection churn on the Redis server with client connection reuse.
- Consistent hashing based on `Jump Consistent Hash algorithm` developed by Google
- Use murmur2 as hashing algorithm
- Failover over multiple nodes in the same shard (useful when using [KeyDb][1] in `active-active` replication and `multi-master` mode)

Key distribution
----------------

Keys are hashed using `murmur2` and distributed using the `Jump Consistent Hash algorithm` across the shards. In case a shard has multiple nodes,
we send the command first to the node with index `Hash rem length(NodesTags)`. In case that ndoe is down the other nodes in the shard
are tried till one is up or no other nodes remaining. 

Quick start
------------

Compile:

```
rebar3 compile
```

You need to define your pools inside `app.config` :

```erlang
{eredis_pool, [
    {pools, [
        {pool1, [
            {shards, [
                <<"shard01node01.bucharest.nbz:6379,shard01node02.bucharest.nbz:6379">>,
                <<"shard02node01.bucharest.nbz:6379,shard02node02.bucharest.nbz:6379">>,
                <<"shard03node01.bucharest.nbz:6379,shard03node02.bucharest.nbz:6379">>,
            ]},

            {connections_per_host, 5},
            {connection_options, [
                {database, 0},
                {reconnect_sleep, 1000}
            ]}
        ]}
    ]}
]}.
```

The shards are specified using the `host:port` format. In the sam shard multiple nodes are accepted separated by comma.
For example in case you are using [KeyDb][1] in `active-active` replication and `multi-master` mode this it's very utils.
In case one replica is down the client will discuss with the next one available.

The supported options for a pool are:

- `database` : database number (integer, default to `0`)
- `password` : database password (string, default empty password)
- `reconnect_sleep`: in case connection to server is lost after how many milliseconds should try to reconnect (integer, default to `100`)
- `connect_timeout`: how many milliseconds will stay in connect until timeout (integer, default `5000`)
- `socket_options`: list of socket options sent to `gen_tcp`.

Executing a query:

```erl
{ok, <<"OK">>} = eredis_pool:q(pool1, [<<"SET">>, Key, Value, <<"EX">>, 10000]).
```

Performance testing
-----------

The code is in `test` folder. In the following test I send 600000 requests from 600 concurrent processes. Each host having 3 connections.

```
make bench
### run test ->  concurrent clients: 600 requests per client: 1000 
### 2241 ms 333333 req/sec 
```

You can run it yourself using `make bench` after you copy the `load_test.erl` from benchmark folder in src and compile.

[1]:https://github.com/JohnSully/KeyDB
[2]:https://github.com/silviucpp/erlpool
[3]:https://github.com/wooga/eredis
