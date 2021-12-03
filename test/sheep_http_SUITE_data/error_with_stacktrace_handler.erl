-module(error_with_stacktrace_handler).
-behaviour(sheep_http).

-export([init/2, exception_handler/4, read/2]).

-include("sheep.hrl").

-spec init(cowboy_req:req(), term()) -> tuple().
init(Req, Opts) ->
    {sheep_http, Req, Opts}.


-spec exception_handler(sheep_request(), atom(), term(), erlang:stracktrace()) -> sheep_response().
exception_handler(_Request, throw, test_exception, Stacktrace) ->
    BinStacktrace = unicode:characters_to_binary(io_lib:format("~p", [Stacktrace])),
    Data = #{<<"error">> => <<"Test exception">>, <<"stacktrace">> => BinStacktrace},
    #sheep_response{body = Data}.


-spec read(sheep_request(), term()) -> sheep_response().
read(#sheep_request{bindings = #{<<"user_id">> := <<"5">>}}, _State)->
    throw(test_exception);

read(_Request, _State)->
    Data = [],
    #sheep_response{status_code = 200, body = Data}.
