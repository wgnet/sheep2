-module(users_handler).

-export([init/3]).

-export([
    read/1,
    read/2
]).

-include("sheep.hrl").

init(_Transport, Req, _Opts) ->
    {upgrade, protocol, sheep_http, Req, []}.


% Get collections
read(_Request)->
    Data = {[
        {<<"key">>, <<"value">>}
    ]},
    #sheep_response{status_code=200, body=Data}.

% Get item
read(_Request, Bindings)->
    Data = {[
        {<<"key">>, <<"value">>}
    ]},
    #sheep_response{status_code=200, body=Data}.