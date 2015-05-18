# Sheep

Cowboy protocol and set of utility funs for building JSON/MsgPack APIs

# Motivation

Cowboy already includes protocol "cowboy_rest", but it has a several disadvantages:

* complexity - more simple behaviour is need in many cases
* error handling is not flexible
* no logging


# User guide

* Initialization
* Options
    * methods_spec
    * content_types_spec
    * access_log_format
* Error handling
    * error_handler/3
    * error_handler/2
* Handlers

## Initialization

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

## Options

### methods_spec

Specifications for methods. Default value is:

```erlang
[
    {<<"POST">>, [create]},
    {<<"GET">>, [read]},
    {<<"PUT">>, [update]},
    {<<"DELETE">>, [delete]}
]
```

### content_types_spec

> Need to implement

Specification of calback for decode/encode data for the appropriate
**content-type**. Default value is:

```erlang
[
    {
        <<"application/json">>,
        fun jiffy:decode/1,
        fun jiffy:encode/1}
    {
        <<"application/x-msgpack">>,
        fun(Payload) ->
            {ok, ParamsMsgPack} = msgpack:unpack(Payload, [{format, jiffy}]),
            ParamsMsgPack
        end,
        fun(Payload) ->
            msgpack:pack(Payload, [{format, jiffy}])
        end
    }
]
```

### access_log_format

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

## Error handling

There are two kinds of error handlers: with arity 2 and 3.


### error_handler/3

Called when handler returns atom **error** and record **sheep_response**


For example, if handler returns following tuple

```erlang
{error, #sheep_response{status_code=400, body= <<"Message">>}};
```

Following pattern matching may be used in error handler:

```erlang
error_handler(#sheep_request{}, 400, #sheep_response{body= <<"Message">>}) ->
```


### error_handler/2

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

## Handlers

For each of any http methods (GET, POST, PUT and etc.) in option parameter *methods_spec* (see above)
you can specify list of functions that must be called. 

Each of this function is should be return one of following values:

* {noreply, State} - for this case will be called next function from list
* {error, _} - for this case will be called appropriate **error_handler** (see above)
* {ok, #sheep_response{}} - This result will be considered as final and it should be returns as response for request

For example if you specify **methods_spec** with following value:

```erlang
{
    methods_spec, [
        {<<"POST">>, [authorization, validation, create]},
        {<<"GET">>, [authorization, read]},
        {<<"PUT">>, []},
        {<<"DELETE">>, []}
    ]
}
```

By request with methods **PUT** or **DELETE** will be returned response with status
code [405](http://httpstatus.es/405) (Method not allowed)

For request with **POST** or **GET** methods you can define callback functions
following way:

```erlang

authorization(State, Request) ->
    Token = sheep_http:get_header(<<"token">>, Request),
    case is_authorized(Token) of
        false ->
            % for breaking chains of callbacks and call error_handler
            {error, #sheep_response{
                        status_code=401, body= <<"Invalid token">>}};
        true ->
            % for to continue processing of request
            {noreply, State#state{counter = State#state.counter + 1}}
    end.

validation(State, Request) ->
    % code for validate
    case Result of
        error ->
            {error, #sheep_response{
                        status_code=400, body= <<"Validation failed">>}};
        ok ->
            {noreply, State#state{counter = State#state.counter + 1}}
    end.

% Get specific item of user
read(_State, #sheep_request{bindings=[{user_id, ID}]})->
    Data = {[
        {<<"id">>, ID},
        {<<"name">>, <<"Username 1">>}
    ]},
    {ok, #sheep_response{status_code=200, body=Body}};

% Get collection
read(_State, _Request)->
    Data = [
        {[
            {<<"id">>, <<"1">>},
            {<<"name">>, <<"Username 1">>}
        ]},
        {[
            {<<"id">>, <<"2">>},
            {<<"name">>, <<"Username 2">>}
        ]}
    ],
    {ok, #sheep_response{status_code=200, body=Data}}.

% Create new item
create(_State, _Request)->
    Data = {[
        {<<"id">>, <<"100">>},
        {<<"name">>, <<"New username">>}
    ]},
    {ok, #sheep_response{status_code=201, body=Data}}.
```

> If all callbacks in chain are returned "noreply" then will be returned
> response with status code [204](http://httpstatus.es/204) (No content)
