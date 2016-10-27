-module(full_users_handler).

-export([init/3, sheep_init/2, my_read/2 ]).

-include("sheep.hrl").

init(_Transport, Req, _Opts) ->
    {upgrade, protocol, sheep_http, Req, []}.

-spec sheep_init(sheep_request(), any()) -> {map(), any()}.
sheep_init(_Request, _Opts) ->
    Options =
        #{
            methods_spec =>
            #{
                <<"GET">> => [my_read]
            }
        },
    State = [],
    {Options, State}.


-spec my_read(sheep_request(), any()) -> {ok, sheep_response()}.
my_read(#{bindings := #{<<"user_id">> := _} = Bindings}, _State)->
    {ok, sheep_http:response(#{status_code => 200, body => Bindings})};

my_read(_Request, _State)->
    Data = #{<<"key">> => <<"value">>},
    {ok, sheep_http:response(#{status_code => 200, body => Data})}.
