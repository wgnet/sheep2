# Sheep

Cowboy protocol and set of utility funs for building JSON/MsgPack APIs

## Motivation

Cowboy already includes protocol "cowboy_rest", but it has a several disadvantages:

* complexity - more simple behaviour is need in many cases
* error handling is not flexible
* no logging


## User guide

* Initialization
* Options
    * methods_spec
    * decode_spec
    * encode_spec
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

Second, the **sheep_init/2** callback will be called where you can set options and state.

```erlang
-spec sheep_init(sheep_request(), any()) -> {list(), any()}.
sheep_init(Request, Opts) ->
    Options = [],
    State = [],
    {Options, State}.
```

## Options

### methods_spec

Specifications for methods. Default value is:

```erlang
#{
    <<"POST">> => [create],
    <<"GET">> => [read],
    <<"PUT">> => [update],
    <<"DELETE">> => [delete]
}.
```

### decode_spec

Specification of calback for decode data for the appropriate **content-type**.

Default value is:

```erlang
[
    {?CT_APP_JSON,
        fun(Payload)
            jiffy:decode(Payload, [return_maps])
        end},
    {?CT_APP_X_MSGPACK,
        fun(Payload) ->
            {ok, Data} = msgpack:unpack(Payload, [{format, map}]),
            Data
        end}
]
```

In case if the **content-type** header is not specified incoming data will be
considered as *json* - encoded

See test handler [encode_decode_handler.erl](./test/sheep_http_SUITE_data/encode_decode_handler.erl)
as example how this can be used

### encode_spec

Specification of calback for encode data for the appropriate **accept**.

Default value is:

```erlang
[
    {?CT_APP_JSON, fun jiffy:encode/1},
    {?CT_APP_X_MSGPACK,
        fun(Payload) ->
            msgpack:pack(Payload, [{format, map}])
        end}
]
```

In case if the **accept** header is not specified response will be encoded to *json*


## Error handling

There are two kinds of error handlers: with arity 2 and 3. And also special handler
for exceptions.


### error_handler/3

Called when handler returns atom **error** and map **sheep_response**


For example, if handler returns following map

```erlang
{error, #{status_code:=400, body:= <<"Message">>}};
```

Following pattern matching may be used in error handler:

```erlang
error_handler(#{}, 400, #{body:= <<"Message">>}) ->
```


### error_handler/2

This is more general error handler that will called when handler return custom error.

For example, if handler returns following tuple

```erlang
{error, {my_error, []}};
```

Following pattern matching may be used in error handler:

```erlang
error_handler(#{}, {my_error, []}) ->
```

### exception_handler/2

This is a special kind of error handler that called when occurs exceptions

For case when handler raise exception

```erlang
throw(my_exception)
```

You can use following pattern matching:

```erlang
exception_handler(#{}, {throw, my_exception}) ->
```

## Handlers

For each of any http methods (GET, POST, PUT and etc.) in option parameter *methods_spec* (see above)
you can specify list of functions that must be called.

Each function should return one of the following values:

* {continue, State} - for this case the next function from list will be called
* {continue, Response, State}
* #sheep_response{} - This result will be considered as final and it should be returned as response for request

For example if you specify **methods_spec** with following value:

```erlang
{
    methods_spec, [
        {<<"POST">>, [authorization, validation, create]},
        {<<"GET">>, [authorization, fun http_helpers:pagination/2, read]},
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
            {error, sheep_response:new(401, <<"Invalid token">>)};
        true ->
            % for to continue processing of request
            {noreply, State#state{counter = State#state.counter + 1}}
    end.

validation(State, Request) ->
    % code for validate
    case Result of
        error ->
            {error, sheep_response:new(400, <<"Validation failed">>)};
        ok ->
            {noreply, State#state{counter = State#state.counter + 1}}
    end.

% Get specific item of user
read(_State, #{bindings:=[{user_id, ID}]})->
    Data = {[
        {<<"id">>, ID},
        {<<"name">>, <<"Username 1">>}
    ]},
    {ok, sheep_response:new(200, Body)};

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
    {ok, sheep_response:new(200, Data)}.

% Create new item
create(_State, _Request)->
    Data = {[
        {<<"id">>, <<"100">>},
        {<<"name">>, <<"New username">>}
    ]},
    {ok, sheep_response:new(201, Data)}.
```

> If all callbacks in chain are returned "noreply" then will be returned
> response with status code [204](http://httpstatus.es/204) (No content)
