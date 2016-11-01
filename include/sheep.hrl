-type http_code() :: integer().

-record(sheep_request, {
    method:: binary(),
    headers = [] :: list(),
    query = #{} :: map(),
    bindings = #{} :: map(),
    body = <<>> :: map() | binary()
}).

-record(sheep_response, {
    status_code = 500 :: http_code(),
    headers = [] :: list(),
    body = <<>> :: map() | binary()
}).

-record(sheep_options, {
    encode_spec :: undefined | map(),
    decode_spec :: undefined | map(),
    method_spec :: undefined | map()
}).
