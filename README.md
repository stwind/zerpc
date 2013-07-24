# zerpc - ZeroMQ Erlang RPC

[![Build Status](https://travis-ci.org/stwind/zerpc.png)](https://travis-ci.org/stwind/zerpc)

RPC through [ZeroMQ](http://www.zeromq.org/) for Erlang.

## Motivation

Erlang cluster is great, but having every nodes in a big cluster and share same cookie is troublesome. 

In a SOA style infracture, we may have many services that built with erlang, each of them may be a standalone cluster, but all services don't necessarily to be of a same huge cluster. So we want to replace the erlang  built-in rpc to something else. 

This is where [ZerpMQ](http://www.zeromq.org/) comes in.

## Overview

Zerpc is an erlang application, there are two modes it can run with: `server` and `client`. They can be run simultaneously.

Server:

```
erl -pa ebin -env ERL_LIBS deps -sname server -setcookie server -zerpc mode server -zerpc endpoint '"tcp://127.0.0.1:5566"' -zerpc size 10
Erlang R15B02 (erts-5.9.2) [source] [64-bit] [smp:4:4] [async-threads:0] [hipe] [kernel-poll:false] [dtrace]

Eshell V5.9.2  (abort with ^G)
(server@localhost)1> application:start(zerpc).
ok
(server@localhost)2> node().
server@localhost
(server@localhost)3> erlang:get_cookie().
server
```

Here we start zerpc in server mode, with cookie set to `server`.

Client:

```
erl -pa ebin -env ERL_LIBS deps -sname client -setcookie client -zerpc mode client -zerpc pools '[{p1,[{endpoint,"tcp://127.0.0.1:5566"},{size,5}]}]'
Erlang R15B02 (erts-5.9.2) [source] [64-bit] [smp:4:4] [async-threads:0] [hipe] [kernel-poll:false] [dtrace]

Eshell V5.9.2  (abort with ^G)
(client@localhost)1> application:start(zerpc).
ok
(client@localhost)2> zerpc:call(p1, erlang, node, []).
server@localhost
(client@localhost)3> zerpc:call(p1, erlang, get_cookie, []).
server
(client@localhost)4> nodes().
[]
```

And we start another vm, and start zerpc in client mode, with cookie `client`. Then we remotely call server's functions using similary semantic to erlang's built-in rpc. And they are not in the same cluster.

## Settings

```erlang
{zerpc, [
    {client, [
        {pools, [
            {zep1, [
                {send_timeout, 5000},
                {recv_timeout, 5000},
                {endpoint, "tcp://127.0.0.1:5566"},
                {size, 5}
            ]}
        ]}
    ]},
    {server, [
            {endpoint, "tcp://*:5566"},
            {size, 5}
        ]}
    ]}
```

## Development

Compile
```
make
```

Run test cases
```
make test
```

Run in dev mode
```
./start-dev.sh
```

## Authors

* [stwind](https://github.com/stwind/)




