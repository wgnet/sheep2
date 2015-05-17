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
    error_handler/2,
    error_handler/3
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

    IsInit = erlang:function_exported(Handler, sheep_init, 2),

    Response = try
        {SheepOpts, State} = case IsInit of
            true ->
                Handler:sheep_init(Request, HandlerOpts);
            false -> {[], []}
        end,
        handle(
            SheepOpts, State,
            Request#sheep_request{
                body = body_params(Req, ContentType)},
            Handler, HandlerOpts)
    catch
        throw:{sheep, #sheep_response{status_code=StatusCode}=ErrorResponse} ->
            handle_error(
                Request, [Request, StatusCode, ErrorResponse], Handler);
        Class:Reason ->
            handle_error(Request, [Request, {Class, Reason}], Handler)
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
            % $remote_addr
            inet:ntoa(element(1, element(1, cowboy_req:peer(Req)))),
            % $host
            element(1, cowboy_req:header(<<"host">>, Req, <<"-">>)),
            % $request
            element(1, cowboy_req:method(Req)),
            element(1, cowboy_req:path(Req)),
            element(1, cowboy_req:version(Req)),
            % $status
            StatusCode,
            % $http_user_agent
            element(1, cowboy_req:header(<<"user-agent">>, Req, <<"-">>))
        ]).

response_204() ->
    sheep_response(204, <<"">>).

response_405() ->
    sheep_response(405, <<"Method not allowed">>).

response_500() ->
    sheep_response(500, <<"Not implemented">>).

response_501() ->
    sheep_response(501, <<"Internal server error">>).


sheep_response(StatusCode, Message) ->
    #sheep_response{status_code=StatusCode, body=Message}.

call_handlers(_State, _Request, _Module, []) ->
    throw({sheep, response_204()});

call_handlers(State, Request, Module, [HandlerFun|Handlers]) ->
    Fun = erlang:function_exported(Module, HandlerFun, 2),
    Result = case Fun of
        true -> 
            Module:HandlerFun(State, Request);
        _ ->
            throw({sheep, response_501()})
    end,
    case Result of
        {noreply, NewState} ->
            call_handlers(NewState, Request, Module, Handlers);
        _ ->
            Result
    end.

-spec handle(list(), any(), #sheep_request{}, module(), any()) -> #sheep_response{}.
handle(Opts, State, Request, HandlerModule, _HandlerOpts) ->
    MethodsSpec = proplists:get_value(methods_spec, Opts, ?PROTOCOL_METHODS_SPEC),
    FindHandlers = lists:keyfind(Request#sheep_request.method, 1, MethodsSpec),

    Result = case FindHandlers of
        false ->
            throw({sheep, response_405()});
        [] ->
            throw({sheep, response_405()});
        {_, Handlers} when is_list(Handlers) ->
            call_handlers(State, Request, HandlerModule, Handlers)
    end,
    case Result of
        {ok, OkResponse} ->
            generate_payload(
                OkResponse,
                get_header(<<"accept">>, Request, ?CT_JSON));
        {error, ErrorResponse} ->
            case ErrorResponse of
                #sheep_response{
                    status_code=StatusCode}
                when is_integer(StatusCode) ->
                    handle_error(
                        Request,
                        [Request, StatusCode, ErrorResponse],
                        HandlerModule);
                _ ->
                    handle_error(
                        Request,
                        [Request, {error, ErrorResponse}],
                        HandlerModule)
            end
    end.


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

-spec error_handler(#sheep_request{}, integer(), #sheep_response{})
    -> #sheep_response{}.
error_handler(_Request, _StatusCode, Response) ->
    Response.

-spec error_handler(#sheep_request{}, {atom(), any()}) -> #sheep_response{}.
error_handler(_Request, _Exception) ->
    error_logger:error_report(erlang:get_stacktrace()),
    response_500().


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
                _:_ -> throw({
                    sheep,
                    sheep_response(400, <<"Can't decode JSON payload">>)})
            end;
        ?CT_MSG_PACK ->
            try
                {ok, ParamsMsgPack} = msgpack:unpack(Payload, [{format, jiffy}]),
                ParamsMsgPack
            catch
                _:_ -> throw({
                    sheep,
                    sheep_response(400, <<"Can't decode MsgPack payload">>)})
            end;
        _ ->
            throw({
                sheep,
                sheep_response(400, <<"Not supported 'content-type'">>)})
    end.

-spec generate_payload(json_obj(), mime_type()) -> iolist().
generate_payload(Response, ContentType) ->
    Data = Response#sheep_response.body,
    Body = case ContentType of
        ?CT_MSG_PACK ->
            try
                msgpack:pack(Data, [{format, jiffy}])
            catch
                _:_ -> throw({
                    sheep,
                    sheep_response(500, <<"Can't encode MsgPack payload">>)})
            end;
        _AnyOtherContentType -> 
            try
                jiffy:encode(Data)
            catch
                _:_ -> throw({
                    sheep,
                    sheep_response(500, <<"Can't encode JSON payload">>)})
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
