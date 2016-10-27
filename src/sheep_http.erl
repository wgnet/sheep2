-module(sheep_http).
-behaviour(cowboy_sub_protocol).

-export([upgrade/4, get_header/2, get_header/3, response/1]).

-include("sheep.hrl").

-define(MIME_JSON, <<"application/json">>).
-define(MIME_MSGPACK, <<"application/x-msgpack">>).


%%% Module API

-spec upgrade(cowboy_req:req(), cowboy_middleware:env(), module(), term()) ->
                     {ok, cowboy_req:req(), cowboy_middleware:env()}.
upgrade(CowRequest, Env, Handler, HandlerOpts) ->
    Request = #{
        meta => [],
        method => element(1, cowboy_req:method(CowRequest)),
        headers => element(1, cowboy_req:headers(CowRequest)),
        bindings => to_map(element(1, cowboy_req:bindings(CowRequest))),
        query => to_map(element(1, cowboy_req:qs_vals(CowRequest))),
        body => #{}
    },

    {SheepOpts, Response} =
        try
            {Options, State} =
                case erlang:function_exported(Handler, sheep_init, 2) of
                    true -> Handler:sheep_init(Request, HandlerOpts);
                    false -> {#{}, []}
                end,
            Request2 = decode_payload(CowRequest, Request, Options),
            {Options, handle(Request2, Handler, Options, State)}
        catch
            throw:{sheep, #{status_code := StatusCode} = ErrorResponse} ->
                {#{}, handle_error(Handler, [Request, StatusCode, ErrorResponse])};
            Class:Reason ->
                {#{}, handle_exception(Handler, [Request, {Class, Reason}])}
        end,

    #{
        status_code := ResponseCode,
        headers := ResponseHeaders,
        body := Body
    } = encode_payload(Request, Response, SheepOpts),

    {ok, CowResponse} = cowboy_req:reply(ResponseCode, ResponseHeaders, Body, CowRequest),
    log_response(CowResponse, ResponseCode),
    {ok, CowResponse, Env}.


-spec get_header(binary(), sheep_request()) -> binary() | undefined.
get_header(Name, Request) ->
    get_header(Name, Request, undefined).

-spec get_header(binary(), sheep_request(), binary() | undefined) -> binary() | undefined.
get_header(Name, #{headers := Headers}, Default) ->
    case lists:keyfind(Name, 1, Headers) of
        {_, Value} -> Value;
        false -> Default
    end.


-spec response(map()) -> sheep_response().
response(Data) ->
    maps:merge(#{
        status_code => 204,
        headers => [],
        body => #{}
    }, Data).


%%% Inner functions

-spec decode_payload(cowboy_req:req(), sheep_request(), list()) -> sheep_request().
decode_payload(CowReq, Request, SheepOpts) ->
    RawContentType = get_header(<<"content-type">>, Request),
    CleanContentType = clean_content_type(RawContentType),
    DecodeSpec = maps:get(decode_spec, SheepOpts, default_decode_spec()),

    Fn = case maps:find(CleanContentType, DecodeSpec) of
             {ok, F} -> F;
             error ->
                 error_logger:info_report([
                     {error, "not supported"},
                     {<<"content-type">>, RawContentType}
                 ]),
                 throw({sheep, sheep_response:new(415, <<"Not supported 'content-type'">>)})
         end,

    case cowboy_req:has_body(CowReq) of
        true ->
            {ok, Body, _} = cowboy_req:body(CowReq),
            try
                Request#{body := Fn(Body)}
            catch
                Class:Reason ->
                    error_logger:info_report([
                        {payload, Body},
                        {description, "can't decode payload"},
                        {exception, {Class,Reason}},
                        {stacktrace, erlang:get_stacktrace()}
                    ]),
                    E = <<"Can't decode '", RawContentType/binary, "' payload">>,
                    throw({sheep, sheep_response:new(400, E)})
            end;
        false -> Request
    end.


-spec encode_payload(sheep_request(), sheep_request(), list()) -> sheep_request().
encode_payload(Request, #{body := Data, headers := Headers} = Response, SheepOpts) ->
    AcceptContentType = get_header(<<"content-type">>, Request),
    EncodeSpec = maps:get(encode_spec, SheepOpts, default_encode_spec()),

    case maps:find(AcceptContentType, EncodeSpec) of
        {ok, Fn} ->
            try
                Headers2 = lists:keystore(
                    <<"content-type">>, 1, Headers,
                    {<<"content-type">>, AcceptContentType}
                ),
                Response#{
                    headers := Headers2,
                    body := Fn(Data)
                }
            catch
                Class:Reason ->
                    error_logger:info_report([
                        {error, encode_payload},
                        {payload, Data},
                        {description, "can't encode payload"},
                        {exception, {Class,Reason}},
                        {stacktrace, erlang:get_stacktrace()}
                    ]),
                    E = <<"Can't encode '", AcceptContentType/binary, "' payload">>,
                    sheep_response:new(500, E)
            end;
        error ->
            sheep_response:new(406, <<"Not acceptable">>)
    end.


-spec handle(sheep_request(), module(), list(), term()) -> sheep_response().
handle(#{method := Method} = Request, HandlerModule, SheepOpts, State) ->
    MethodsSpec = maps:get(methods_spec, SheepOpts, default_method_spec()),

    Result =
        case maps:find(Method, MethodsSpec) of
            {ok, Handlers} when is_list(Handlers) ->
                call_handlers(Request, HandlerModule, Handlers, State);
            error ->
                throw({sheep, sheep_response:new_405()})
        end,
    case Result of
        {ok, OkResponse} -> OkResponse;
        {error, #{status_code := StatusCode} = ErrorResponse} when is_integer(StatusCode) ->
            handle_error(HandlerModule, [Request, StatusCode, ErrorResponse]);
        {error, ErrorResponse} ->
            handle_error(HandlerModule, [Request, ErrorResponse]);
        InvalidResponse ->
            handle_error(HandlerModule, [Request, {invalid, InvalidResponse}])
    end.


call_handlers(_Request, _Module, [], _State) ->
    throw({sheep, sheep_response:new_204()});


call_handlers(Request, Module, [HandlerFun|Handlers], State) ->
    Fun = case erlang:is_function(HandlerFun, 2) of
              true -> HandlerFun;
              _ ->
                  case erlang:function_exported(Module, HandlerFun, 2) of
                      true -> fun Module:HandlerFun/2;
                      _ -> throw({sheep, sheep_response:new_501()})
                  end
          end,
    case Fun(Request, State) of
        {noreply, NewState} ->
            call_handlers(Request, Module, Handlers, NewState);
        {noreply, NewRequest, NewState} ->
            call_handlers(NewRequest, Module, Handlers, NewState);
        Result ->
            Result
    end.


handle_error(Handler, Args) ->
    handle_error(Handler, error_handler, Args).


handle_error(Handler, Fn, Args) ->
    case erlang:function_exported(Handler, Fn, length(Args)) of
        true ->
            try
                apply(Handler, Fn, Args)
            catch
                Class:Reason ->
                    ST = erlang:get_stacktrace(),
                    error_logger:error_report([
                        {error, invalid_handler},
                        {handler, Handler},
                        {exception, {Class, Reason}},
                        {stacktrace, ST}
                    ]),
                    sheep_response:new_500()
            end;
        false ->
            case {Fn, Args} of
                {error_handler, [_Request, _StatusCode, Response]} -> Response;
                {error_handler, [_Request, Error]} ->
                    ST = erlang:get_stacktrace(),
                    error_logger:error_report([{error, Error}, {stacktrace, ST}]),
                    sheep_response:new_500();
                {exception_handler, [_Request, Exception]} ->
                    ST = erlang:get_stacktrace(),
                    error_logger:error_report([{exception, Exception}, {stacktrace, ST}]),
                    sheep_response:new_500()
            end
    end.


handle_exception(Handler, Args) ->
    handle_error(Handler, exception_handler, Args).


default_decode_spec() ->
    #{
        ?MIME_JSON =>
        fun(Payload) ->
            jiffy:decode(Payload, [return_maps])
        end,
        ?MIME_MSGPACK =>
        fun(Payload) ->
            {ok, Data} = msgpack:unpack(Payload, [{format, map}]),
            Data
        end
    }.


default_encode_spec() ->
    #{
        ?MIME_JSON =>
        fun(Payload) ->
            jiffy:encode(Payload ,[pretty])
        end,
        ?MIME_MSGPACK =>
        fun(Payload) ->
            msgpack:pack(Payload, [{format, map}])
        end
    }.


default_method_spec() ->
    #{
        <<"POST">> => [create],
        <<"GET">> => [read],
        <<"PUT">> => [update],
        <<"DELETE">> => [delete]
    }.


-spec clean_content_type(binary() | undefined) -> binary() | undefined.
clean_content_type(undefined) -> undefined;
clean_content_type(RawContentType) ->
    %% http://www.w3.org/Protocols/rfc1341/4_Content-Type.html
    case binary:split(RawContentType, <<";">>) of
        [ContentType, _Parameters] -> ContentType;
        [ContentType] -> ContentType
    end.


-spec to_map([proplists:property()]) -> map().
to_map(List) ->
    maps:from_list(
      lists:map(fun({K, V}) -> {to_binary(K), V} end, List)
    ).

to_binary(V) when is_atom(V) -> to_binary(atom_to_list(V));
to_binary(V) when is_list(V) -> list_to_binary(V);
to_binary(V) when is_binary(V) -> V.

-spec log_response(cowboy_req:req(), http_code()) -> atom().
log_response(Req, StatusCode) ->
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
