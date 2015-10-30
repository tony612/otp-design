# gen_server 行为

这节应该配合着 `stdlib` 中的手册 [gen_server(3)](http://www.erlang.org/doc/man/gen_server.html)
一起读，所有的接口函数和回调函数都在手册中有详细定义。

## 2.1 客户端-服务端原则

客户端-服务端模型一般由一个中心服务端和一些客户端组成，它被用于资源管理操作。其中，几个不同的客户端
共享一个相同的资源，服务端负责管理这个资源。

![client server](./clientserver.gif)

*图 2.1： 客户端-服务端模型*

## 2.2 举例

一个用普通 Erlang 写的简单的服务端在 [Overview](./overview.md) 中给出了，那个服务端可以用 `gen_server`
来实现，于是有这个回调函数：

```erlang
-module(ch3).
-behaviour(gen_server).

-export([start_link/0]).
-export([alloc/0, free/1]).
-export([init/1, handle_call/3, handle_cast/2]).

start_link() ->
    gen_server:start_link({local, ch3}, ch3, [], []).

alloc() ->
    gen_server:call(ch3, alloc).

free(Ch) ->
    gen_server:cast(ch3, {free, Ch}).

init(_Args) ->
    {ok, channels()}.

handle_call(alloc, _From, Chs) ->
    {Ch, Chs2} = alloc(Chs),
    {reply, Ch, Chs2}.

handle_cast({free, Ch}, Chs) ->
    Chs2 = free(Ch, Chs),
    {noreply, Chs2}.
```

代码在下边的几节被解释。

## 2.3 启动一个 Gen_Server

在上一节的例子当中，`gen_server` 通过调用 `ch3:start_link()` 来启动：

```erlang
start_link() ->
    gen_server:start_link({local, ch3}, ch3, [], []) => {ok, Pid}
```

start_link 调用函数 `gen_server:start_link/4`，这个函数衍生并且连接到一个新的进程，一个 `gen_server`。
