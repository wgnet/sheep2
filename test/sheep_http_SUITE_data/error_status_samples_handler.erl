-module(error_status_samples_handler).
-behaviour(sheep_http).

-export([init/2, exception_handler/3, read/2
]).

-include("sheep.hrl").

-spec init(cowboy_req:req(), term()) -> tuple().
init(Req, Opts) ->
    {sheep_http, Req, Opts}.


-spec exception_handler(#sheep_request{}, atom(), term()) -> #sheep_response{}.
exception_handler(_Request, throw, test_exception) ->
    Data = #{<<"error">> => <<"Test exception">>},
    #sheep_response{status_code = 400, body = Data}.


-spec read(#sheep_request{}, term()) -> #sheep_response{}.
read(#sheep_request{bindings = #{<<"user_id">> := <<"3">>}}, _State)->
    Body = #{<<"error">> => <<"simple_error">>},
    #sheep_response{status_code = 400, body = Body};

read(#sheep_request{bindings = #{<<"user_id">> := <<"4">>}}, _State)->
    Body = #{<<"error">> => <<"custom_error">>},
    #sheep_response{body = Body};

read(#sheep_request{bindings = #{<<"user_id">> := <<"5">>}}, _State)->
    throw(test_exception);

read(#sheep_request{bindings = #{<<"user_id">> := _UserID}}, _State)->
    #sheep_response{status_code = 404, body = <<"Not found">>};

read(_Request, _State)->
    Data = [
        #{<<"id">> => <<"1">>, <<"name">> => <<"Username 1">>},
        #{<<"id">> => <<"2">>, <<"name">> => <<"Username 2">>}
    ],
    #sheep_response{status_code = 200, body = Data}.
