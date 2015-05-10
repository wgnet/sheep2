-module(users_handler).

-export([init/3]).

-export([
    read/2,
    read/3
]).

-include("sheep.hrl").

init(_Transport, Req, _Opts) ->
    {upgrade, protocol, sheep_http, Req, []}.

-spec sheep_init(#sheep_request{}, any()) -> {list(), any()}.
sheep_init(Request, Opts) ->
    {[
        {methods_spec, [
            {<<"POST">>, create},
            {<<"GET">>, read},
            {<<"PUT">>, update},
            {<<"DELETE">>, delete}
        ]}
    ],
    []}.

% Get collections
read(State, _Request)->
    Data = {[
        {<<"key">>, <<"value">>}
    ]},
    #sheep_response{status_code=200, body=Data}.

% Get item
read(State, _Request, Bindings)->
    Data = {[
        {<<"key">>, <<"value">>}
    ]},
    #sheep_response{status_code=200, body=Data}.