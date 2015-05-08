%% Usefull links
% 
% http://jsonapi.org/
% http://www.restapitutorial.com/
% http://restful-api-design.readthedocs.org/en/latest/
% http://restless.readthedocs.org/en/latest/tutorial.html#creating-updating-deleting-data

% Problems with sheep
% 
% - complexity of exceptions
% - internal exceptions can be obtained in user error handlers 
% - stacktrace concatenation - is not convenient because 
%   the real error can be contained in the middle of it
% - complexity of hanlers (arity 5)
% - not used functions for validation (may be deleted)
% - not support authorization/validation (callbacks)
% - no logging
% - unexpected normalized parameters (and extra overhead for normalization)
% - no differentiation by type of resource (collection/object)

-module(sheep_http).


-behaviour(cowboy_sub_protocol).

-export([upgrade/4]).

-export([
    get_header/2,
    get_header/3,
    error_handler/1,
    error_handler/2
]).

-include("sheep.hrl").

-spec upgrade(cowboy_req:req(), cowboy_middleware:env(), module(), any()) ->
                     {ok, cowboy_req:req(), cowboy_middleware:env()}.
upgrade(Req, Env, Handler, HandlerOpts) ->
    {ContentType, _} = cowboy_req:header(<<"content-type">>, Req, ?CT_JSON),
    {Method, _} = cowboy_req:method(Req),
    {Bindings, _} = cowboy_req:bindings(Req),
    {Query, _} = cowboy_req:qs_vals(Req),
    {Headers, _} = cowboy_req:headers(Req),

    Request = #sheep_request{
        method = Method,
        headers = Headers,
        bindings = Bindings,
        query = Query
    },
    Response = try
        handle(
            Request#sheep_request{
                body = body_params(Req, ContentType)},
            Handler, HandlerOpts)
    catch
        throw:{sheep, StatusCode, Message} ->
            handle_error(Request, [StatusCode, Message], Handler);
        Class:Reason ->
            handle_error(Request, [{Class, Reason}], Handler)
    end,
    {ok, CowResponse} = cowboy_req:reply(
        Response#sheep_response.status_code,
        Response#sheep_response.headers,
        Response#sheep_response.body, Req),
    logging_request(CowResponse, Response#sheep_response.status_code),
    {ok, CowResponse, Env}.

-spec logging_request(cowboy_req:req(), http_code()) -> atom().
logging_request(Req, StatusCode) ->
    % TODO: Provide ability to specify format for logging in handler module
    error_logger:info_msg(
        <<"[http] ~s ~s - \"~s ~s ~s\" ~w ~s">>, [
            inet:ntoa(element(1, element(1, cowboy_req:peer(Req)))),
            element(1, cowboy_req:header(<<"host">>, Req, <<"-">>)),
            element(1, cowboy_req:path(Req)),
            element(1, cowboy_req:method(Req)),
            element(1, cowboy_req:version(Req)),
            StatusCode,
            element(1, cowboy_req:header(<<"user-agent">>, Req, <<"-">>))
        ]).

-spec handle(#sheep_request{}, module(), any()) -> #sheep_response{}.
handle(Request, Handler, _HandlerOpts) ->
    Method = lists:keyfind(
        Request#sheep_request.method, 1, ?PROTOCOL_METHODS_SPEC),
    Response = case Method of
        false ->
            throw({sheep, 405, <<"Method not allowed">>});
        {_, HandlerFun} ->
            F1 = erlang:function_exported(Handler, HandlerFun, 1),
            F2 = erlang:function_exported(Handler, HandlerFun, 2),
            case {Request#sheep_request.bindings, F1, F2} of
                {[], true, _} -> 
                    Handler:HandlerFun(Request);
                {Bindings, _, true} -> 
                    Handler:HandlerFun(Request, Bindings);
                {_, false, false}->
                    throw({sheep, 405, <<"Method not allowed">>});
                _ ->
                    throw({sheep, 501, <<"Not implemented">>})
            end
    end,
    generate_payload(
        Response,
        get_header(<<"accept">>, Request, ?CT_JSON)).


handle_error(Request, Args, Handler) ->
    Fn = erlang:function_exported(Handler, error_handler, length(Args)),
    case Fn of
        true ->
            try
                call_error_handler(Request, Handler, Args)
            catch
                _:HandlerError ->
                    error_logger:error_msg(
                        <<"error handler: ~p, error: ~p">>,[Handler, HandlerError]),
                    call_error_handler(Request, ?MODULE, Args)
            end;
        false ->
            call_error_handler(Request, ?MODULE, Args)
    end.

call_error_handler(Request, Handler, Args)->
    generate_payload(
        apply(Handler, error_handler, Args),
        get_header(<<"accept">>, Request, ?CT_JSON)).

-spec error_handler(integer(), binary()) -> #sheep_response{}.
error_handler(StatusCode, Message) ->
    #sheep_response{
        status_code=StatusCode,
        body=Message
    }.

error_handler(_Exception) ->
    error_logger:error_report(erlang:get_stacktrace()),
    #sheep_response{
        status_code = 500,
        body = <<"Internal server error">>
    }.


-spec body_params(cowboy_req:req(), binary()) -> {json_obj(), cowboy_req:req()}.
body_params(Req, ContentType) ->
    case cowboy_req:has_body(Req) of
        true ->
            {ok, Body, _} = cowboy_req:body(Req),
            parse_payload(Body, ContentType);
        false -> {[]}
    end.

-spec parse_payload(binary(), mime_type()) -> json_obj().
parse_payload(Payload, ContentType) ->
    case ContentType of
        ?CT_JSON ->
            try
                jiffy:decode(Payload)
            catch
                _:_ -> throw({sheep, 400, <<"Can't decode JSON payload">>})
            end;
        ?CT_MSG_PACK ->
            try
                {ok, ParamsMsgPack} = msgpack:unpack(Payload, [{format, jiffy}]),
                ParamsMsgPack
            catch
                _:_ -> throw({sheep, 400, <<"Can't decode MsgPack payload">>})
            end;
        _ ->
            throw({sheep, 400, <<"Not supported 'content-type'">>})
    end.

-spec generate_payload(json_obj(), mime_type()) -> iolist().
generate_payload(Response, ContentType) ->
    Data = Response#sheep_response.body,
    Body = case ContentType of
        ?CT_MSG_PACK ->
            try
                msgpack:pack(Data, [{format, jiffy}])
            catch
                _:_ -> throw({sheep, 500, <<"Can't encode MsgPack payload">>})
            end;
        _AnyOtherContentType -> 
            try
                jiffy:encode(Data)
            catch
                _:_ -> throw({sheep, 500, <<"Can't encode JSON payload">>})
            end
    end,
    % TODO: extend headers instead replace
    Response#sheep_response{
        headers = [{<<"content-type">>, ContentType}],
        body = Body}.

-spec get_header(binary(), #sheep_request{}) -> binary().
get_header(Name, Request) ->
    get_header(Name, Request, undefined).

-spec get_header(binary(), #sheep_request{}, any()) -> binary().
get_header(Name, Request, Default) ->
    case lists:keyfind(Name, 1, Request#sheep_request.headers) of
        {_, Value} -> Value;
        false -> Default
    end.
