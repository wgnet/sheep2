-module(users_handler).

-export([init/3]).

-export([
    sheep_init/2,
    error_handler/3,
    error_handler/2,
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

error_handler(Request, 400, Response) ->
    Data = {[
        {<<"error">>, Response#sheep_response.body}
    ]},
    Response#sheep_response{body=Data}.

error_handler(Request, {throw, test_exception}) ->
    Data = {[
        {<<"error">>, <<"Test exception">>}
    ]},
    #sheep_response{status_code=400, body=Data};

error_handler(Request, {error, {custom_error, Message}}) ->
    Data = {[
        {<<"error">>, Message}
    ]},
    #sheep_response{status_code=400, body=Data}.

% Get collections
read(State, _Request)->
    Data = {[
        {<<"key">>, <<"value">>}
    ]},
    {ok, #sheep_response{status_code=200, body=Data}}.

% Get item
read(State, _Request, [{user_id, <<"throw_id">>}])->
    throw(test_exception),
    ok;

read(State, _Request, [{user_id, <<"error_id">>}])->
    {error, #sheep_response{status_code=400, body= <<"Message">>}};

read(State, _Request, [{user_id, <<"custom_error_id">>}])->
    {error, {custom_error, <<"Message">>}};

read(State, _Request, Bindings)->
    Data = {Bindings},
    {ok, #sheep_response{status_code=200, body=Data}}.