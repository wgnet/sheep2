-module(simple_handler).
-behaviour(sheep_http).

-export([init/3, read/2]).

-include("sheep.hrl").

init(_Transport, Req, _Opts) ->
    {upgrade, protocol, sheep_http, Req, []}.


-spec read(sheep_request(), any()) -> {ok, sheep_response()}.
read(#{bindings := #{<<"param">> := <<"empty_1">>}} = _Request, _State) ->
    {ok, sheep_http:response(#{status_code => 204, body => <<>>})};

read(#{bindings := #{<<"param">> := <<"empty_2">>}} = _Request, _State) ->
    {ok, sheep_http:response(#{status_code => 204, body => #{}})};

read(#{body := Body}, _State)->
    {ok, sheep_http:response(#{status_code => 200, body => Body})}.
