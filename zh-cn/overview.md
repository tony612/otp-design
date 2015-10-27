# 总览

**OTP 设计原则** 描述了如何从进程、模块和目录的方面来组织 Erlang 的代码。

## 1.1 监督树

Erlang/OTP 的一个基本概念就是 **监督树(supervision tree)**。它是一个基于 **工作者(workers)** 和 **监督者(supervisors)** 的进程组织模型：

  * 工作者是执行计算的进程，也就是说，它们做实际的工作
  * 监督者是监控工作者行为的进程，一个监督者能够在一个工作者出错时重启它。
  * 监督树是把代码分成监督者和工作者的层级排列，这让我们能够设计和编写出容错的软件。

下边这个图中，方形代表监督者，圆形代表了工作者：

![supervision tree](./supervision_tree.gif)

*图 1.1：监督树*

## 1.2 行为

在监督树中，许多进程有相似的结构，它们遵循相似的模式。比如，监督者在结构上是相似的，它们之间唯一的区别是它们监督的子进程。
许多工作者是服务端-客户端关系、有限状态机或者事件处理器（比如错误日志记录器）这样的服务器。

**行为（Behaviours）** 是这些常见模式的规范化。思路就是把一个进程的代码分为通有部分（行为模块）和具体部分（回调模块）。

下边这个例子说明了，代码怎样能够被划分为通有部分和具体部分。考虑这段写了一个简单的服务器的代码（用普通的 Erlang 写的），
它记录了很多 “信道”，其他进程能够通过调用函数 `alloc/0` 和 `free/1` 来分配或者释放 “信道”。

```erlang
-module(ch1).
-export([start/0]).
-export([alloc/0, free/1]).
-export([init/0]).

start() ->
    spawn(ch1, init, []).

alloc() ->
    ch1 ! {self(), alloc},
    receive
        {ch1, Res} ->
            Res
    end.

free(Ch) ->
    ch1 ! {free, Ch},
    ok.

init() ->
    register(ch1, self()),
    Chs = channels(),
    loop(Chs).

loop(Chs) ->
    receive
        {From, alloc} ->
            {Ch, Chs2} = alloc(Chs),
            From ! {ch1, Ch},
            loop(Chs2);
        {free, Ch} ->
            Chs2 = free(Ch, Chs),
            loop(Chs2)
    end.
```

服务器的代码能被重写成一个通用部分 `server.erl`：

```erlang
-module(server).
-export([start/1]).
-export([call/2, cast/2]).
-export([init/1]).

start(Mod) ->
    spawn(server, init, [Mod]).

call(Name, Req) ->
    Name ! {call, self(), Req},
    receive
        {Name, Res} ->
            Res
    end.

cast(Name, Req) ->
    Name ! {cast, Req},
    ok.

init(Mod) ->
    register(Mod, self()),
    State = Mod:init(),
    loop(Mod, State).

loop(Mod, State) ->
    receive
        {call, From, Req} ->
            {Res, State2} = Mod:handle_call(Req, State),
            From ! {Mod, Res},
            loop(Mod, State2);
        {cast, Req} ->
            State2 = Mod:handle_cast(Req, State),
            loop(Mod, State2)
    end.
```

和一个回调模块 `ch2.erl`：

```erlang
-module(ch2).
-export([start/0]).
-export([alloc/0, free/1]).
-export([init/0, handle_call/2, handle_cast/2]).

start() ->
    server:start(ch2).

alloc() ->
    server:call(ch2, alloc).

free(Ch) ->
    server:cast(ch2, {free, Ch}).

init() ->
    channels().

handle_call(alloc, Chs) ->
    alloc(Chs). % => {Ch,Chs2}

handle_cast({free, Ch}, Chs) ->
    free(Ch, Chs). % => Chs2
```
