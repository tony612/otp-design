# 监督者行为

这节应该和 STDLIB 中的 [supervisor(3)](http://www.erlang.org/doc/man/supervisor.html)
手册一起阅读，其中有关于监督者行为的所有细节。

## 5.1 监督原则

一个监督者会负责对它的子进程进行启动、停止和监控。一个监督者的基本思想是，通过在必要的时候重启
子进程来让它们保持存在的状态。

## 5.2 例子

一个启动 [gen_server 行为](./gen_server.md) 中的服务端的回调模块可能是这样的：

```erlang
-module(ch_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link(ch_sup, []).

init(_Args) ->
    SupFlags = #{strategy => one_for_one, intensity => 1, period => 5},
    ChildSpecs = [#{id => ch3,
                    start => {ch3, start_link, []},
                    restart => permanent,
                    shutdown => brutal_kill,
                    type => worker,
                    modules => [cg3]}],
    {ok, {SupFlags, ChildSpecs}}.
```

`init/1` 返回值中的 `SupFlags` 变量代表 [监督者标记](#监督者标记)。

而返回值中的 `ChildSpecs` 变量是 [子进程规范](#子进程规范) 的队列。
