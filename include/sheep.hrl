% Types

-type(json_obj() :: any()).
-type(mime_type() :: binary()).
-type(http_code() :: integer()).


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
    {?CT_APP_JSON, fun jiffy:encode/1},
    {?CT_APP_X_MSGPACK, fun(Payload) -> msgpack:pack(Payload, [{format, jiffy}]) end}
]).

-define(PROTOCOL_DECODE_SPEC, [
    {?CT_APP_JSON, fun jiffy:decode/1},
    {?CT_APP_X_MSGPACK, fun(Payload) -> msgpack:unpack(Payload, [{format, jiffy}]) end}
]).

-record(sheep_request, {
    meta = [],
    method = undefined,
    headers = [],
    bindings = {[]},
    query = {[]},
    body = {[]} :: json_obj()
}).

-record(sheep_response, {
    status_code,
    headers = [],
    body = {[]}
}).
