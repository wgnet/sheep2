-module(status_samples_handler).
-behaviour(sheep_http).

-export([init/3, read/2]).

-include("sheep.hrl").

-spec init(atom(), cowboy_req:req(), term()) -> tuple().
init(_Transport, Req, _Opts) ->
    {upgrade, protocol, sheep_http, Req, []}.


-spec read(#sheep_request{}, term()) -> #sheep_response{}.
read(#sheep_request{bindings = #{<<"user_id">> := <<"1">>}}, _State) ->
    Body = #{<<"id">> => <<"1">>, <<"name">> => <<"Username 1">>},
    #sheep_response{status_code = 200, body = Body};

read(#sheep_request{bindings = #{<<"user_id">> := <<"2">>}}, _State) ->
    #sheep_response{status_code = 204, body = <<>>};

read(#sheep_request{bindings = #{<<"user_id">> := <<"3">>}}, _State) ->
    #sheep_response{status_code = 204, body = #{}};

read(#sheep_request{bindings = #{<<"user_id">> := <<"5">>}}, _State)->
    throw(test_exception);

read(#sheep_request{bindings = #{<<"user_id">> := _}}, _State)->
    sheep_response:new_404();

read(_Request, _State)->
    Body = [
        #{<<"id">> => <<"1">>, <<"name">> => <<"Username 1">>},
        #{<<"id">> => <<"2">>, <<"name">> => <<"Username 2">>}
    ],
    #sheep_response{status_code = 200, body = Body}.
