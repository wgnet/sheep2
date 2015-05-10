-module(users_handler).

-export([init/3]).

-export([
    sheep_init/2,
    error_handler/1,
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

error_handler({throw, test_exception}) ->
    Data = {[
        {<<"key">>, <<"value">>}
    ]},
    #sheep_response{status_code=400, body=Data}.

% Get collections
read(State, _Request)->
    Data = {[
        {<<"key">>, <<"value">>}
    ]},
    #sheep_response{status_code=200, body=Data}.

% Get item
read(State, _Request, [{user_id, <<"throw_id">>}])->
    throw(test_exception),
    ok;
    

read(State, _Request, Bindings)->
    Data = {[
        {<<"key">>, <<"value">>}|Bindings
    ]},
    #sheep_response{status_code=200, body=Data}.