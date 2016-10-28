-module(status_samples_handler).
-behaviour(sheep_http).

-export([init/3, read/2]).

-include("sheep.hrl").

init(_Transport, Req, _Opts) ->
    {upgrade, protocol, sheep_http, Req, []}.


-spec read(sheep_request(), term()) -> sheep_response().
read(#{bindings := #{<<"user_id">> := <<"1">>}}, _State) ->
    Body = #{<<"id">> => <<"1">>, <<"name">> => <<"Username 1">>},
    sheep_http:response(#{status_code => 200, body => Body});

read(#{bindings := #{<<"user_id">> := <<"2">>}}, _State) ->
    sheep_http:response(#{status_code => 204, body => <<>>});

read(#{bindings := #{<<"user_id">> := <<"3">>}}, _State) ->
    sheep_http:response(#{status_code => 204, body => #{}});

read(#{bindings := #{<<"user_id">> := _}}, _State)->
    sheep_response:new_404();

read(_Request, _State)->
    Body = [
        #{<<"id">> => <<"1">>, <<"name">> => <<"Username 1">>},
        #{<<"id">> => <<"2">>, <<"name">> => <<"Username 2">>}
    ],
    sheep_http:response(#{status_code => 200, body => Body}).
