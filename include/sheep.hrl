-type http_code() :: integer().

-record(sheep_request, {
    method :: binary(),
    path :: binary(),
    headers = #{} :: map(),
    query = #{} :: map(),
    bindings = #{} :: map(),
    body = <<>> :: map() | [map()] | binary(),
    peer :: {inet:ip_address(), inet:port_number()} | undefined
}).
-type sheep_request() :: #sheep_request{}.

-record(sheep_response, {
    status_code = 500 :: http_code(),
    headers = #{} :: map(),
    body = <<>> :: map() | [map()] | binary() | undefined
}).
-type sheep_response() :: #sheep_response{}.

-record(sheep_options, {
    encode_spec :: undefined | map(),
    decode_spec :: undefined | map(),
    method_spec :: undefined | map()
}).
-type sheep_options() :: #sheep_options{}.
