-module(sheep_http).
-behaviour(cowboy_sub_protocol).

-export([upgrade/4, get_header/2, get_header/3, response/1]).

-include("sheep.hrl").

-callback sheep_init(Request :: sheep_request(), HandlerOpts :: term()) -> {Options :: map(), State :: term()}.
-callback create(Request :: sheep_request(), State :: term()) -> Response :: sheep_response().
-callback read(Request :: sheep_request(), State :: term()) -> Response :: sheep_response().
-callback update(Request :: sheep_request(), State :: term()) -> Response :: sheep_response().
-callback delete(Request :: sheep_request(), State :: term()) -> Response :: sheep_response().
-callback exception_handler(Request :: sheep_request(), Class :: atom(), Reason :: term()) -> sheep_response().

-optional_callbacks([sheep_init/2, create/2, read/2, update/2, delete/2, exception_handler/3]).

-define(MIME_JSON, <<"application/json">>).
-define(MIME_MSGPACK, <<"application/x-msgpack">>).


%%% Module API

-spec upgrade(cowboy_req:req(), cowboy_middleware:env(), module(), term()) ->
                     {ok, cowboy_req:req(), cowboy_middleware:env()}.
upgrade(CowRequest, Env, Handler, HandlerOpts) ->
    Request = #{
        method => element(1, cowboy_req:method(CowRequest)),
        headers => element(1, cowboy_req:headers(CowRequest)),
        bindings => to_map(element(1, cowboy_req:bindings(CowRequest))),
        query => to_map(element(1, cowboy_req:qs_vals(CowRequest))),
        body => case cowboy_req:has_body(CowRequest) of
                    true -> {ok, Body0, _} = cowboy_req:body(CowRequest), Body0;
                    false -> <<>>
                end
    },

    {Options, Response} =
        try
            {Options0, State} =
                case erlang:function_exported(Handler, sheep_init, 2) of
                    true -> Handler:sheep_init(Request, HandlerOpts);
                    false -> {#{}, []}
                end,
            case decode_payload(Request, Options0) of
                {ok, Request2} ->
                    Response0 = handle(Request2, Handler, Options0, State),
                    {Options0, Response0};
                {error, Response0} -> {Options0, Response0}
            end
        catch
            Class:Reason ->
                {#{}, handle_exception(Handler, Request, Class, Reason)}
        end,
    #{
        status_code := ResponseCode,
        headers := ResponseHeaders,
        body := Body
    } = encode_payload(Request, Response, Options),

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
        status_code => 500,
        headers => [],
        body => <<>>
    }, Data).


%%% Inner functions

-spec decode_payload(sheep_request(), map()) -> {ok, sheep_request()} | {error, sheep_response()}.
decode_payload(#{body := <<>>} = Request, _Options) ->
    {ok, Request};
decode_payload(#{body := Body} = Request, Options) ->
    RawContentType = get_header(<<"content-type">>, Request),
    CleanContentType = clean_content_type(RawContentType),
    DecodeSpec = maps:get(decode_spec, Options, default_decode_spec()),

    case maps:find(CleanContentType, DecodeSpec) of
        {ok, Fn} ->
            try
                {ok, Request#{body := Fn(Body)}}
            catch
                Class:Reason ->
                    ST = erlang:get_stacktrace(),
                    error_logger:info_report([
                        {error, decode_payload},
                        {payload, Body},
                        {description, "can't decode payload"},
                        {exception, {Class,Reason}},
                        {stacktrace, ST}
                    ]),
                    E = <<"Can't decode '", RawContentType/binary, "' payload">>,
                    {error, sheep_response:new(400, E)}
            end;
        error ->
            error_logger:info_report([
                {error, "not supported"},
                {<<"content-type">>, RawContentType}
            ]),
            {error, sheep_response:new(415, <<"Not supported 'content-type'">>)}
    end.


-spec encode_payload(sheep_request(), sheep_request(), map()) -> sheep_response().
encode_payload(_Request, #{body := Body} = Response, _Options) when is_binary(Body) ->
    Response;
encode_payload(_Request, #{status_code := 204} = Response, _Options) ->
    Response#{body => <<>>};
encode_payload(Request, #{body := Body, headers := Headers} = Response, Options) ->
    AcceptContentType = get_header(<<"accept">>, Request),
    EncodeSpec = maps:get(encode_spec, Options, default_encode_spec()),

    case maps:find(AcceptContentType, EncodeSpec) of
        {ok, Fn} ->
            try
                Headers2 = lists:keystore(
                    <<"content-type">>, 1, Headers,
                    {<<"content-type">>, AcceptContentType}
                ),
                Body2 = Fn(Body),
                Response#{
                    headers := Headers2,
                    body := Body2
                }
            catch
                Class:Reason ->
                    ST = erlang:get_stacktrace(),
                    error_logger:info_report([
                        {error, encode_payload},
                        {payload, Body},
                        {description, "can't encode payload"},
                        {exception, {Class,Reason}},
                        {stacktrace, ST}
                    ]),
                    E = <<"Can't encode '", AcceptContentType/binary, "' payload">>,
                    sheep_response:new(500, E)
            end;
        error ->
            error_logger:info_report([
                {error, "not acceptable"},
                {<<"content-type">>, AcceptContentType}
            ]),
            sheep_response:new(406, <<"Not acceptable">>)
    end.


-spec handle(sheep_request(), module(), map(), term()) -> sheep_response().
handle(#{method := Method} = Request, HandlerModule, SheepOpts, State) ->
    MethodsSpec = maps:get(methods_spec, SheepOpts, default_method_spec()),
    case maps:find(Method, MethodsSpec) of
        {ok, Handlers} when is_list(Handlers) ->
            call_handlers(Request, HandlerModule, Handlers, State);
        error ->
            {ok, sheep_response:new_405()}
    end.


-spec call_handlers(sheep_request(), atom(), list(), term()) -> sheep_response().
call_handlers(_Request, _Module, [], _State) ->
    sheep_response:new_204();

call_handlers(Request, Module, [HandlerFun|Handlers], State) ->
    Fun = case erlang:is_function(HandlerFun, 2) of
              true -> HandlerFun;
              _ ->
                  case erlang:function_exported(Module, HandlerFun, 2) of
                      true -> fun Module:HandlerFun/2;
                      _ -> sheep_response:new_501()
                  end
          end,
    case Fun(Request, State) of
        {noreply, NewState} ->
            call_handlers(Request, Module, Handlers, NewState);
        {noreply, NewRequest, NewState} ->
            call_handlers(NewRequest, Module, Handlers, NewState);
        Result -> Result
    end.


-spec handle_exception(atom(), sheep_request(), atom(), term()) -> sheep_response().
handle_exception(Handler, Request, Class, Reason) ->
    case erlang:function_exported(Handler, exception_handler, 3) of
        true ->
            try
                apply(Handler, exception_handler, [Request, Class, Reason])
            catch
                Class:Reason ->
                    ST = erlang:get_stacktrace(),
                    error_logger:error_report([
                        {error, invalid_exception_handler},
                        {handler, Handler},
                        {exception, {Class, Reason}},
                        {stacktrace, ST}
                    ]),
                    sheep_response:new_500()
            end;
        false ->
            ST = erlang:get_stacktrace(),
            error_logger:error_report([
                {handler, Handler},
                {exception, {Class, Reason}},
                {stacktrace, ST}
            ]),
            sheep_response:new_500()
    end.


default_decode_spec() ->
    #{
        ?MIME_JSON =>
        fun(Payload) ->
            jiffy:decode(Payload, [return_maps])
        end,
        ?MIME_MSGPACK =>
        fun(Payload) ->
            {ok, Data} = msgpack:unpack(Payload, [{map_format, map}]),
            Data
        end
    }.


default_encode_spec() ->
    #{
        ?MIME_JSON =>
        fun(Payload) ->
            jiffy:encode(Payload, [pretty])
        end,
        ?MIME_MSGPACK =>
        fun(Payload) ->
            msgpack:pack(Payload, [{map_format, map}])
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
    error_logger:info_msg("[http] ~s ~s - \"~s ~s ~s\" ~w ~s",
        [
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
