-module(basic_users_handler_with_error_handler).

-export([
    init/3
]).

% Error handlers
-export([
    error_handler/3,
    error_handler/2
]).

% Request handlers
-export([
    read/2
]).

-include("sheep.hrl").

init(_Transport, Req, _Opts) ->
    {upgrade, protocol, sheep_http, Req, []}.


% Standart error handler for sheep_response
error_handler(_Request, 400, Response) ->
    Data = {[
        {<<"error">>, Response#sheep_response.body}
    ]},
    Response#sheep_response{body=Data}.

% --- Generic error handler ---

% handling of custom error
error_handler(_Request, {error, {custom_error, Message}}) ->
    Data = {[
        {<<"error">>, Message}
    ]},
    #sheep_response{status_code=400, body=Data};
% handling of exception
error_handler(_Request, {throw, test_exception}) ->
    Data = {[
        {<<"error">>, <<"Test exception">>}
    ]},
    #sheep_response{status_code=400, body=Data}.


% Get specific user
read(#sheep_request{bindings=[{user_id, <<"error_id">>}]}, _State)->
    Body = <<"Error message">>,
    {error, #sheep_response{status_code=400, body=Body}};

read(#sheep_request{bindings=[{user_id, <<"custom_error_id">>}]}, _State)->
    {error, {custom_error, <<"Error message">>}};

read(#sheep_request{bindings=[{user_id, <<"throw_id">>}]}, _State)->
    throw(test_exception),
    ok;

read(#sheep_request{bindings=[{user_id, _}]}, _State)->
    Body = <<"Not found">>,
    {ok, #sheep_response{status_code=404, body=Body}};

% Get collection
read(_Request, _State)->
    Data = [
        {[
            {<<"id">>, <<"1">>},
            {<<"name">>, <<"Username 1">>}
        ]},
        {[
            {<<"id">>, <<"2">>},
            {<<"name">>, <<"Username 2">>}
        ]}
    ],
    {ok, #sheep_response{status_code=200, body=Data}}.