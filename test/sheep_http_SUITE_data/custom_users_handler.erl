-module(custom_users_handler).
-behaviour(sheep_http).

-export([init/3, exception_handler/3, read/2
]).

-include("sheep.hrl").

init(_Transport, Req, _Opts) ->
    {upgrade, protocol, sheep_http, Req, []}.


-spec exception_handler(sheep_request(), atom(), term()) -> sheep_response().
exception_handler(_Request, throw, test_exception) ->
    Data = #{<<"error">> => <<"Test exception">>},
    sheep_http:response(#{status_code => 400, body => Data}).


-spec read(sheep_request(), term()) -> sheep_response().
read(#{bindings := #{<<"user_id">> := <<"simple_error">>}}, _State)->
    Body = #{<<"error">> => <<"simple_error">>},
    sheep_http:response(#{status_code => 400, body => Body});

read(#{bindings := #{<<"user_id">> := <<"custom_error">>}}, _State)->
    Body = #{<<"error">> => <<"custom_error">>},
    sheep_http:response(#{body => Body});

%%read(#{bindings := #{<<"user_id">> := <<"custom_error_id">>}}, _State)->
%%    {error, {custom_error, <<"Error message">>}};
%%
%%read(#{bindings := #{<<"user_id">> := <<"throw_id">>}}, _State)->
%%    throw(test_exception);

read(#{bindings := #{<<"user_id">> := _UserID}}, _State)->
    sheep_http:response(#{status_code => 404, body => <<"Not found">>});

read(_Request, _State)->
    Data = [
        #{<<"id">> => <<"1">>, <<"name">> => <<"Username 1">>},
        #{<<"id">> => <<"2">>, <<"name">> => <<"Username 2">>}
    ],
    sheep_http:response(#{status_code => 200, body => Data}).
