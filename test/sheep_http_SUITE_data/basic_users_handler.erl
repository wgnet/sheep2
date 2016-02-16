-module(basic_users_handler).

-export([init/3, read/2]).


init(_Transport, Req, _Opts) ->
    {upgrade, protocol, sheep_http, Req, []}.


read(#{bindings := #{<<"user_id">> := _}}, _State)->
    {error, sheep_response:new_404()};

read(_, _State)->
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
    {ok, sheep_http:response(#{status_code => 200, body => Data})}.
