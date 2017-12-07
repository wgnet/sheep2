-module(invalid_init_3_handler).
-behaviour(sheep_http).

-export([init/2, sheep_init/2, exception_handler/3]).

-include("sheep.hrl").

-spec init(cowboy_req:req(), term()) -> tuple().
init(Req, Opts) ->
    {sheep_http, Req, Opts}.


-spec sheep_init(#sheep_request{}, term()) -> {#sheep_options{}, term()}.
sheep_init(_Request, _Opts) ->
    throw(test_exception),
    {#sheep_options{}, []}.


-spec exception_handler(#sheep_request{}, atom(), term()) -> #sheep_response{}.
exception_handler(_Request, throw, test_exception) ->
    throw(another_test_exception),
    #sheep_response{status_code = 400}.
