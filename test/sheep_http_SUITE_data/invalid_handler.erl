-module(invalid_handler).
-behaviour(sheep_http).

-export([init/2, sheep_init/2 ]).

-include("sheep.hrl").

-spec init(cowboy_req:req(), term()) -> tuple().
init(Req, Opts) ->
    {sheep_http, Req, Opts}.


-spec sheep_init(#sheep_request{}, term()) -> {#sheep_options{}, term()}.
sheep_init(_Request, _Opts) ->
    Options =
        #sheep_options{
            method_spec =
            #{
                <<"GET">> => [],
                <<"POST">> => [create]
            }
        },
    {Options, []}.
