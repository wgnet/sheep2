-module(invalid_init_handler).
-behaviour(sheep_http).

-export([init/3, sheep_init/2 ]).

-include("sheep.hrl").

-spec init(atom(), cowboy_req:req(), term()) -> tuple().
init(_Transport, Req, _Opts) ->
    {upgrade, protocol, sheep_http, Req, []}.


-spec sheep_init(sheep_request(), term()) -> {map(), term()}.
sheep_init(_Request, _Opts) ->
    throw(test_exception),
    {#{}, []}.

