-type http_code() :: integer().

-type sheep_request() :: #{
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


