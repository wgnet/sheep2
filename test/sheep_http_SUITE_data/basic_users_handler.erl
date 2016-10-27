-module(basic_users_handler).

-export([init/3, read/2]).

-include("sheep.hrl").

init(_Transport, Req, _Opts) ->
    {upgrade, protocol, sheep_http, Req, []}.


-spec read(sheep_request(), any()) -> {ok, sheep_response()} | {error, sheep_response()}.
read(#{bindings := #{<<"user_id">> := _}}, _State)->
    {error, sheep_response:new_404()};

read(_Request, _State)->
    Data = [
        #{
            <<"id">> => <<"1">>,
            <<"name">> => <<"Username 1">>
        },
        #{
            <<"id">> => <<"2">>,
            <<"name">> => <<"Username 2">>
        }
    ],
    {ok, sheep_http:response(#{status_code => 200, body => Data})}.
