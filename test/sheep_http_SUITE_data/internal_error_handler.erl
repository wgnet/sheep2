-module(internal_error_handler).
-behaviour(sheep_http).

-export([init/3, create/2, exception_handler/3]).

-include("sheep.hrl").

-spec init(atom(), cowboy_req:req(), term()) -> tuple().
init(_Transport, Req, _Opts) ->
    {upgrade, protocol, sheep_http, Req, []}.


create(#sheep_request{bindings = #{<<"type">> := <<"response_encode_error">>}}, _State)->
    Body = #{<<"error">> => self()},
    #sheep_response{status_code = 200, body = Body};
create(_, _) ->
    #sheep_response{status_code = 200, body = #{}}.





exception_handler(_Request, sheep_internal_error, Details) ->
    {Code, Body} = handle_internal_error(Details),
    #sheep_response{status_code = Code, body = jiffy:encode(Body)}.

handle_internal_error({request_decode_error = T, Class, _Reason, RawContentType}) ->
    {400, #{type => T,
            class => Class,
            content_type => RawContentType}};
handle_internal_error({unsupported_content_type = T, RawContentType}) ->
    {415, #{type => T,
            content_type => RawContentType}};
handle_internal_error({response_encode_error  =T, Class, _Reason, AcceptContentType}) ->
    {500, #{type => T,
            class => Class,
            content_type => AcceptContentType}};
handle_internal_error({unsupported_accept = T, AcceptContentType}) ->
    {406, #{type => T,
            content_type => AcceptContentType}};
handle_internal_error(method_not_allowed = T) ->
    {405, #{type => T}};
handle_internal_error({handler_callback_missing = T, Module, Handler}) ->
    {501, #{type => T,
            module => Module,
            handler => Handler}}.
