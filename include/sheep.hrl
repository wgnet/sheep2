% Types

-type http_code() :: integer().

-type sheep_request() :: #{
                     meta => list(),
                     method => binary(),
                     headers => list(),
                     query => map(),
                     bindings => map(),
                     body => map() | binary()
                    }.

-type sheep_response() :: #{
                      status_code => http_code(),
                      headers => list(),
                      body => map() | binary()
                     }.


-define(CT_APP_JSON, <<"application/json">>).
-define(CT_APP_X_MSGPACK, <<"application/x-msgpack">>).
-define(CT_APP_XHTML, <<"application/xhtml+xml">>).

% CRUD operations

-define(PROTOCOL_METHODS_SPEC, [
    {<<"POST">>, [create]},
    {<<"GET">>, [read]},
    {<<"PUT">>, [update]},
    {<<"DELETE">>, [delete]}
]).

-define(PROTOCOL_ENCODE_SPEC, [
    {?CT_APP_JSON,
        fun(Payload) ->
            jiffy:encode(Payload ,[pretty])
        end
    },
    {?CT_APP_X_MSGPACK,
        fun(Payload) ->
            msgpack:pack(Payload, [{format, map}])
        end}
]).

-define(PROTOCOL_DECODE_SPEC, [
    {?CT_APP_JSON,
        fun(Payload) ->
            jiffy:decode(Payload, [return_maps])
        end},
    {?CT_APP_X_MSGPACK,
        fun(Payload) ->
            {ok, Data} = msgpack:unpack(Payload, [{format, map}]),
            Data
        end}
]).
