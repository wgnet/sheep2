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


read(#sheep_request{bindings=Bindings}, _State)->
    case maps:find(<<"user_id">>, Bindings) of
        {ok, <<"error_id">>} ->
            {error, #sheep_response{status_code=400, body= <<"Error message">>}};
        {ok, <<"custom_error_id">>} ->
            {error, {custom_error, <<"Error message">>}};
        {ok, <<"throw_id">>} ->
            throw(test_exception);
        {ok, _} ->
            {ok, #sheep_response{status_code=404, body= <<"Not found">>}};
        error ->
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
            {ok, #sheep_response{status_code=200, body=Data}}
    end.
