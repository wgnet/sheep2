-module(invalid_handler).
-behaviour(sheep_http).

-export([init/3, sheep_init/2 ]).

-include("sheep.hrl").

init(_Transport, Req, _Opts) ->
    {upgrade, protocol, sheep_http, Req, []}.


-spec sheep_init(sheep_request(), term()) -> {map(), term()}.
sheep_init(_Request, _Opts) ->
    Options =
        #{
            methods_spec =>
            #{
                <<"GET">> => [],
                <<"POST">> => [create]
            }
        },
    {Options, []}.

