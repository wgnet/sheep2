# Sheep

Cowboy protocol and set of utility funs for building JSON/MsgPack APIs


## Initialization

First you should initialize protocol for cowboy

```erlang
-include("sheep.hrl").

init(_Transport, Req, _Opts) ->
    {upgrade, protocol, sheep_http, Req, []}.
```

Second, the **sheep_init/2** callback will be called where you can set options and state.

```erlang
-spec sheep_init(#sheep_request{}, term()) -> {#sheep_options{}, term()}.
sheep_init(Request, Opts) ->
    Options = #sheep_options{},
    State = #{},
    {Options, State}.
```


## Options

```erlang
-record(sheep_options, {
    encode_spec :: undefined | map(),
    decode_spec :: undefined | map(),
    method_spec :: undefined | map()
}).
```


### decode_spec

Specification of callback for decode data for the appropriate **content-type**.

Default value is:

```erlang
 #{
    <<"application/json">> =>
    fun(Payload) ->
        jiffy:decode(Payload, [return_maps])
    end,
    <<"application/x-msgpack">> =>
    fun(Payload) ->
        {ok, Data} = msgpack:unpack(Payload, [{map_format, map}]),
        Data
    end
}.
```

See [encode_decode_handler.erl](./test/sheep_http_SUITE_data/encode_decode_handler.erl) as example.


### encode_spec

Specification of callback for encode data for the appropriate **accept**.

Default value is:

```erlang
 #{
    <<"application/json">> =>
    fun(Payload) ->
        jiffy:encode(Payload, [pretty])
    end,
    <<"application/x-msgpack">> =>
    fun(Payload) ->
        msgpack:pack(Payload, [{map_format, map}])
    end
}.
```


### method_spec

Specifications for methods. Default value is:

```erlang
 #{
    <<"POST">> => [create],
    <<"GET">> => [read],
    <<"PUT">> => [update],
    <<"DELETE">> => [delete]
}.
```


## Handlers

For each http method (GET, POST, PUT etc) you can specify list of functions that must be called.

Each function should return one of the following values:

* {continue, State} - in this case the next function from list will be called
* {continue, Response, State} - same as above
* #sheep_response{} - This result will be considered as final and returned as response

For example if you specify **method_spec** in following way:

```erlang
 #sheep_options{
    method_spec = #{
        <<"POST">> => [authorization, validation, create],
        <<"GET">> => [authorization, fun http_helpers:pagination/2, read],
        <<"PUT">> => [],
        <<"DELETE">> => []
    }
}
```

then **GET** and **POST** requests will be processed by list of functions
but **PUT** or **DELETE** will get 204 (no content) response.

See [pipeline_handler.erl](./test/sheep_http_SUITE_data/pipeline_handler.erl) as example.
