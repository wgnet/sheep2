-module(simple_handler).
-behaviour(sheep_http).

-export([init/2, read/2, create/2, update/2, delete/2]).

-include("sheep.hrl").

-spec init(cowboy_req:req(), term()) -> tuple().
init(Req, Opts) ->
    {sheep_http, Req, Opts}.


-spec read(sheep_request(), term()) -> sheep_response().
read(#sheep_request{query = #{<<"page">> := Page, <<"order">> := Order}, path = Path}, _State) ->
    Body = #{
        <<"path">> => Path,
        <<"got_page">> => Page,
        <<"got_order">> => Order
    },
    #sheep_response{status_code = 200, body = Body};

read(#sheep_request{path = Path}, _State)->
    Body = #{
        <<"path">> => Path,
        <<"reply_from">> => <<"read">>
    },
    #sheep_response{status_code = 200, body = Body}.


-spec create(sheep_request(), term()) -> sheep_response().
create(_Request, _State)->
    Body = #{<<"reply_from">> => <<"create">>},
    #sheep_response{status_code = 200, body = Body}.


-spec update(sheep_request(), term()) -> sheep_response().
update(_Request, _State)->
    Body = #{<<"reply_from">> => <<"update">>},
    #sheep_response{status_code = 200, body = Body}.


-spec delete(sheep_request(), term()) -> sheep_response().
delete(_Request, _State)->
    Body = #{<<"reply_from">> => <<"delete">>},
    #sheep_response{status_code = 200, body = Body}.
