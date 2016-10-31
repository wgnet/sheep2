-module(simple_handler).
-behaviour(sheep_http).

-export([init/3, read/2, create/2, update/2, delete/2]).

-include("sheep.hrl").

-spec init(atom(), cowboy_req:req(), term()) -> tuple().
init(_Transport, Req, _Opts) ->
    {upgrade, protocol, sheep_http, Req, []}.


-spec read(sheep_request(), term()) -> sheep_response().
read(#{query := #{<<"page">> := Page, <<"order">> := Order}}, _State) ->
    Body = #{<<"got_page">> => Page, <<"got_order">> => Order},
    sheep_http:response(#{status_code => 200, body => Body});

read(_Request, _State)->
    Body = #{<<"reply_from">> => <<"read">>},
    sheep_http:response(#{status_code => 200, body => Body}).


-spec create(sheep_request(), term()) -> sheep_response().
create(_Request, _State)->
    Body = #{<<"reply_from">> => <<"create">>},
    sheep_http:response(#{status_code => 200, body => Body}).


-spec update(sheep_request(), term()) -> sheep_response().
update(_Request, _State)->
    Body = #{<<"reply_from">> => <<"update">>},
    sheep_http:response(#{status_code => 200, body => Body}).


-spec delete(sheep_request(), term()) -> sheep_response().
delete(_Request, _State)->
    Body = #{<<"reply_from">> => <<"delete">>},
    sheep_http:response(#{status_code => 200, body => Body}).
