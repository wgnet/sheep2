% Types

-type(json_obj() :: any()).
-type(mime_type() :: binary()).
-type(http_code() :: integer()).


-define(CT_JSON, <<"application/json">>).
-define(CT_MSG_PACK, <<"application/x-msgpack">>).

% CRUD operations

-define(PROTOCOL_METHODS_SPEC, [
    {<<"POST">>, [create]},
    {<<"GET">>, [read]},
    {<<"PUT">>, [update]},
    {<<"DELETE">>, [delete]}
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
