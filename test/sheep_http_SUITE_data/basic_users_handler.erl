-module(basic_users_handler).

-export([init/3, read/2]).

-include("sheep.hrl").

init(_Transport, Req, _Opts) ->
    {upgrade, protocol, sheep_http, Req, []}.


read(#sheep_request{bindings = #{<<"user_id">> := _}}, _State)->
    {error, #sheep_response{status_code=404, body= <<"Not found">>}};

read(#sheep_request{}, _State)->
    Data = [
            {[
              {<<"id">>, <<"1">>},
              {<<"name">>, <<"Username 1">>}
             ]},
            {[
              {<<"id">>, <<"2">>},
              {<<"name">>, <<"Username 2">>}
             ]}
           ],
    {ok, #sheep_response{status_code=200, body=Data}}.
