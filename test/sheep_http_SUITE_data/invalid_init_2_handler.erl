-module(invalid_init_2_handler).
-behaviour(sheep_http).

-export([init/3, sheep_init/2, exception_handler/3]).

-include("sheep.hrl").

init(_Transport, Req, _Opts) ->
    {upgrade, protocol, sheep_http, Req, []}.


-spec sheep_init(sheep_request(), term()) -> {map(), term()}.
sheep_init(_Request, _Opts) ->
    throw(test_exception),
    {#{}, []}.


-spec exception_handler(sheep_request(), atom(), term()) -> sheep_response().
exception_handler(_Request, throw, test_exception) ->
    Data = #{<<"error">> => <<"Test exception">>},
    sheep_http:response(#{status_code => 400, body => Data}).
