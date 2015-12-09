-module(full_users_handler).

-export([init/3]).

-export([
    sheep_init/2,
    read/2
]).

-include("sheep.hrl").

init(_Transport, Req, _Opts) ->
    {upgrade, protocol, sheep_http, Req, []}.

-spec sheep_init(#sheep_request{}, any()) -> {list(), any()}.
sheep_init(_Request, _Opts) ->
    {[
        {methods_spec, [
            {<<"POST">>, [create]},
            {<<"GET">>, [read]},
            {<<"PUT">>, [update]},
            {<<"DELETE">>, [delete]}
        ]}
    ],
    []}.

read(#sheep_request{bindings = #{<<"user_id">> := _} = Bindings}, _State)->
    {ok, #sheep_response{status_code=200, body=Bindings}};

read(#sheep_request{}, _State)->
    Data = {[
             {<<"key">>, <<"value">>}
            ]},
    {ok, #sheep_response{status_code=200, body=Data}}.
