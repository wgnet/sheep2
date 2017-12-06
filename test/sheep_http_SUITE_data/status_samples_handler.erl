-module(status_samples_handler).
-behaviour(sheep_http).

-export([init/2, read/2]).

-include("sheep.hrl").

-spec init(cowboy_req:req(), term()) -> tuple().
init(Req, Opts) ->
    {sheep_http, Req, Opts}.


-spec read(#sheep_request{}, term()) -> #sheep_response{}.
read(#sheep_request{bindings = #{user_id := <<"1">>}}, _State) ->
    Body = #{<<"id">> => <<"1">>, <<"name">> => <<"Username 1">>},
    #sheep_response{status_code = 200, body = Body};

read(#sheep_request{bindings = #{user_id := <<"2">>}}, _State) ->
    #sheep_response{status_code = 204, body = <<>>};

read(#sheep_request{bindings = #{user_id := <<"3">>}}, _State) ->
    #sheep_response{status_code = 204, body = #{}};

read(#sheep_request{bindings = #{user_id := <<"5">>}}, _State)->
    throw(test_exception);

read(#sheep_request{bindings = #{user_id := _}}, _State)->
    sheep_response:new_404();

read(_Request, _State)->
    Body = [
        #{<<"id">> => <<"1">>, <<"name">> => <<"Username 1">>},
        #{<<"id">> => <<"2">>, <<"name">> => <<"Username 2">>}
    ],
    #sheep_response{status_code = 200, body = Body}.
