# Sheep

Cowboy protocol and set of utility funs for building JSON/MsgPack APIs

## Motivation

Cowboy already includes protocol "cowboy_rest", but it has a several disadvantages:

* complexity - more simple behaviour is need in many cases
* error handling is not flexible
* no logging


## User guide

### Initialization

First you should initialize protocol for cowboy

```erlang
-include("sheep.hrl").

init(_Transport, Req, _Opts) ->
    {upgrade, protocol, sheep_http, Req, []}.
```

Second, the **sheep_init/2** callback will be called. In it you can set options
and state.

```erlang
-spec sheep_init(#sheep_request{}, any()) -> {list(), any()}.
sheep_init(Request, Opts) ->
    Options = [],
    State = [],
    {Options, State}.
```

#### Options

##### methods_spec

Specifications for methods. Default value is:

```erlang
[
    {<<"POST">>, create},
    {<<"GET">>, read},
    {<<"PUT">>, update},
    {<<"DELETE">>, delete}
]
```

##### access_log_format

> Need to implement

Format of access logs. Available paramaters:

* $remote_addr
* $host
* $request
* $status
* $http_user_agent

Default value is:

```
$remote_addr $host - "$request" $status $http_user_agent
```

### Error handling

There are two kinds of error handlers: with arity 2 and 3.


#### error_handler/3

Called when handler returns atom **error** and record **sheep_response**


For example, if handler returns following tuple

```erlang
{error, #sheep_response{status_code=400, body= <<"Message">>}};
```

Following pattern matching may be used in error handler:

```erlang
error_handler(#sheep_request{}, 400, #sheep_response{body= <<"Message">>}) ->
```


#### error_handler/2

This is more general error handler that will called when exception occurs or 
handler return custom error.

For example, if handler returns following tuple

```erlang
{error, {my_error, []}};
```

Following pattern matching may be used in error handler:

```erlang
error_handler(#sheep_request{}, {error, {my_error, []}}) ->
```

For case when handler raise exception

```erlang
throw(my_exception)
```

You can use following pattern matching:

```erlang
error_handler(#sheep_request{}, {throw, my_exception}) ->
```

