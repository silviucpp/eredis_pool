eredis_pool
=======

[![Build Status](https://travis-ci.org/silviucpp/eredis_pool.svg?branch=master)](https://travis-ci.org/silviucpp/eredis_pool)

An Erlang pool for Redis with consistent hashing based on [erlpool][2] and [eredis][3] 

Features
---------

- Connection pool with persistent connections - this helps reduce connection churn on the Redis server with client connection reuse.
- Consistent hashing based on `Jump Consistent Hash algorithm` developed by Google
- Use murmur2 as hashing algorithm
- Failover over multiple nodes in the same shard (useful when using [KeyDb][1] in `active-active` replication and `multi-master` mode)

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

[1]:https://github.com/JohnSully/KeyDB
[2]:https://github.com/silviucpp/erlpool
[3]:https://github.com/wooga/eredis
