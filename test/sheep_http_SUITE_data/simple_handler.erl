-module(simple_handler).
-behaviour(sheep_http).

-export([init/3, read/2, create/2, update/2, delete/2]).

-include("sheep.hrl").

init(_Transport, Req, _Opts) ->
    {upgrade, protocol, sheep_http, Req, []}.


-spec read(sheep_request(), any()) -> {ok, sheep_response()}.
read(#{bindings := #{<<"param">> := <<"empty_1">>}}, _State) ->
    {ok, sheep_http:response(#{status_code => 204, body => <<>>})};

read(#{bindings := #{<<"param">> := <<"empty_2">>}}, _State) ->
    {ok, sheep_http:response(#{status_code => 204, body => #{}})};

read(#{query := #{<<"page">> := Page, <<"order">> := Order}}, _State) ->
    Body = #{<<"got_page">> => Page, <<"got_order">> => Order},
    {ok, sheep_http:response(#{status_code => 200, body => Body})};

read(_Request, _State)->
    Body = #{<<"reply_from">> => <<"read">>},
    {ok, sheep_http:response(#{status_code => 200, body => Body})}.


create(_Request, _State)->
    Body = #{<<"reply_from">> => <<"create">>},
    {ok, sheep_http:response(#{status_code => 200, body => Body})}.


update(_Request, _State)->
    Body = #{<<"reply_from">> => <<"update">>},
    {ok, sheep_http:response(#{status_code => 200, body => Body})}.


delete(_Request, _State)->
    Body = #{<<"reply_from">> => <<"delete">>},
    {ok, sheep_http:response(#{status_code => 200, body => Body})}.
