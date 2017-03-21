-module(sheep_http).
-behaviour(cowboy_sub_protocol).

-export([upgrade/4, get_header/2, get_header/3]).

-include("sheep.hrl").

-callback sheep_init(Request :: #sheep_request{}, HandlerOpts :: term()) ->
    {Options :: #sheep_options{}, State :: term()}.
-callback create(Request :: #sheep_request{}, State :: term()) -> Response :: #sheep_response{}.
-callback read(Request :: #sheep_request{}, State :: term()) -> Response :: #sheep_response{}.
-callback update(Request :: #sheep_request{}, State :: term()) -> Response :: #sheep_response{}.
-callback delete(Request :: #sheep_request{}, State :: term()) -> Response :: #sheep_response{}.
-callback exception_handler(Request :: #sheep_request{}, Class :: atom(), Reason :: term()) -> #sheep_response{}.

-optional_callbacks([sheep_init/2, create/2, read/2, update/2, delete/2, exception_handler/3]).

-define(MIME_JSON, <<"application/json">>).
-define(MIME_MSGPACK, <<"application/x-msgpack">>).


%%% Module API

-spec upgrade(cowboy_req:req(), cowboy_middleware:env(), module(), term()) ->
                     {ok, cowboy_req:req(), cowboy_middleware:env()}.
upgrade(CowRequest, Env, Handler, HandlerOpts) ->
    {Method, CowRequest1} = cowboy_req:method(CowRequest),
    {Headers, CowRequest2} = cowboy_req:headers(CowRequest1),
    {Bindings, CowRequest3} = cowboy_req:bindings(CowRequest2),
    {Query, CowRequest4} = cowboy_req:qs_vals(CowRequest3),
    {Body, CowRequest5} = case cowboy_req:has_body(CowRequest4) of
                              true ->
                                  {ok, Body0, CowReq5} = cowboy_req:body(CowRequest4),
                                  {Body0, CowReq5};
                              false ->
                                  {<<>>, CowRequest4}
                          end,
    Request = #sheep_request{
        method = Method,
        headers = Headers,
        bindings = to_map(Bindings),
        query = to_map(Query),
        body = Body
    },

    {Options, Response} =
        try
            {Options0, State} =
                case erlang:function_exported(Handler, sheep_init, 2) of
                    true -> Handler:sheep_init(Request, HandlerOpts);
                    false -> {#sheep_options{}, []}
                end,
            case decode_payload(Request, Options0) of
                {ok, Request2} ->
                    Response0 = handle(Request2, Handler, Options0, State),
                    {Options0, Response0};
                {error, Response0} -> {Options0, Response0}
            end
        catch
            Class:Reason ->
                {#sheep_options{}, handle_exception(Handler, Request, Class, Reason)}
        end,
    #sheep_response{
        status_code = ResponseCode,
        headers = ResponseHeaders,
        body = Body
    } = encode_payload(Request, Response, Options),

    {ok, CowResponse} = cowboy_req:reply(ResponseCode, ResponseHeaders, ResponseBody, CowRequest5),
    log_query(CowResponse, ResponseCode),
    {ok, CowResponse, Env}.


-spec get_header(binary(), #sheep_request{}) -> binary() | undefined.
get_header(Name, Request) ->
    get_header(Name, Request, undefined).

-spec get_header(binary(), #sheep_request{}, binary() | undefined) -> binary() | undefined.
get_header(Name, #sheep_request{headers = Headers}, Default) ->
    case lists:keyfind(Name, 1, Headers) of
        {_, Value} -> Value;
        false -> Default
    end.


%%% Inner functions

-spec decode_payload(#sheep_request{}, #sheep_options{}) -> {ok, #sheep_request{}} | {error, #sheep_response{}}.
decode_payload(#sheep_request{body = <<>>} = Request, _Options) ->
    {ok, Request};
decode_payload(#sheep_request{body = Body} = Request, Options) ->
    RawContentType = get_header(<<"content-type">>, Request),
    CleanContentType = clean_content_type(RawContentType),
    DecodeSpec = decode_spec(Options),

    case maps:find(CleanContentType, DecodeSpec) of
        {ok, Fn} ->
            try
                {ok, Request#sheep_request{body = Fn(Body)}}
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
                    {error, #sheep_response{status_code = 400, body = E}}
            end;
        error ->
            error_logger:info_report([
                {error, "not supported"},
                {<<"content-type">>, RawContentType}
            ]),
            {error, #sheep_response{status_code = 415, body = <<"Not supported 'content-type'">>}}
    end.


-spec encode_payload(#sheep_request{}, #sheep_response{}, #sheep_options{}) -> #sheep_response{}.
encode_payload(_Request, #sheep_response{body = Body} = Response, _Options) when is_binary(Body) ->
    Response;
encode_payload(_Request, #sheep_response{status_code = 204} = Response, _Options) ->
    Response#sheep_response{body = <<>>};
encode_payload(_Request, #sheep_response{body = undefined} = Response, _Options) ->
    Response#sheep_response{body = <<>>};
encode_payload(Request, #sheep_response{body = Body, headers = Headers} = Response, Options) ->
    AcceptContentType = get_header(<<"accept">>, Request),
    EncodeSpec = encode_spec(Options),

    case maps:find(AcceptContentType, EncodeSpec) of
        {ok, Fn} ->
            try
                Headers2 = lists:keystore(
                    <<"content-type">>, 1, Headers,
                    {<<"content-type">>, AcceptContentType}
                ),
                Response#sheep_response{
                    headers = Headers2,
                    body = Fn(Body)
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
                    #sheep_response{status_code = 500, body = E}
            end;
        error ->
            error_logger:info_report([
                {error, "not acceptable"},
                {<<"content-type">>, AcceptContentType}
            ]),
            #sheep_response{status_code = 406, body = <<"Not acceptable">>}
    end.


-spec handle(#sheep_request{}, module(), #sheep_options{}, term()) -> #sheep_response{}.
handle(#sheep_request{method = Method} = Request, HandlerModule, Options, State) ->
    MethodsSpec = method_spec(Options),
    case maps:find(Method, MethodsSpec) of
        {ok, Handlers} when is_list(Handlers) ->
            call_handlers(Handlers, Request, HandlerModule, State);
        error ->
            sheep_response:new_405()
    end.


-spec call_handlers(list(), #sheep_request{}, atom(), term()) -> #sheep_response{}.
call_handlers([], _Request, _Module, _State) ->
    sheep_response:new_204();

call_handlers([Handler | Handlers], Request, Module, State) ->
    case erlang:is_function(Handler, 2) of
        true -> call_fun(Handler, Handlers, Request, Module, State);
        _ ->
            case erlang:function_exported(Module, Handler, 2) of
                true -> call_fun(fun Module:Handler/2, Handlers, Request, Module, State);
                _ -> sheep_response:new_501()
            end
    end.


-spec call_fun(function(), list(), #sheep_request{}, atom(), term()) -> #sheep_response{}.
call_fun(Fun, Handlers, Request, Module, State) ->
    case Fun(Request, State) of
        {continue, NewState} ->
            call_handlers(Handlers, Request, Module, NewState);
        {continue, NewRequest, NewState} ->
            call_handlers(Handlers, NewRequest, Module, NewState);
        Result -> Result
    end.


-spec handle_exception(atom(), #sheep_request{}, atom(), term()) -> #sheep_response{}.
handle_exception(Handler, Request, Class, Reason) ->
    case erlang:function_exported(Handler, exception_handler, 3) of
        true ->
            try
                Handler:exception_handler(Request, Class, Reason)
            catch
                Class1:Reason1 ->
                    ST1 = erlang:get_stacktrace(),
                    error_logger:error_report([
                        {error, invalid_exception_handler},
                        {handler, Handler},
                        {exception, {Class1, Reason1}},
                        {stacktrace, ST1}
                    ]),
                    sheep_response:new_500()
            end;
        false ->
            ST2 = erlang:get_stacktrace(),
            error_logger:error_report([
                {handler, Handler},
                {exception, {Class, Reason}},
                {stacktrace, ST2}
            ]),
            sheep_response:new_500()
    end.


-spec decode_spec(#sheep_options{}) -> map().
decode_spec(#sheep_options{decode_spec = Spec}) when is_map(Spec) -> Spec;
decode_spec(#sheep_options{decode_spec = undefined}) ->
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


-spec encode_spec(#sheep_options{}) -> map().
encode_spec(#sheep_options{encode_spec = Spec}) when is_map(Spec) -> Spec;
encode_spec(#sheep_options{encode_spec = undefined}) ->
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


-spec method_spec(#sheep_options{}) -> map().
method_spec(#sheep_options{method_spec = Spec}) when is_map(Spec) -> Spec;
method_spec(#sheep_options{method_spec = undefined}) ->
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


-spec to_binary(atom() | list() | binary()) -> binary().
to_binary(V) when is_atom(V) -> to_binary(atom_to_list(V));
to_binary(V) when is_list(V) -> list_to_binary(V);
to_binary(V) when is_binary(V) -> V.


-spec log_query(cowboy_req:req(), http_code()) -> ok.
log_query(Req, StatusCode) ->
    case application:get_env(sheep2, log_query) of
        {ok, true} -> do_log_query(Req, StatusCode);
        _ -> ignore
    end.

do_log_query(Req, StatusCode) ->
    {{RAddr, _RPort}, _} = cowboy_req:peer(Req),
    RemoteAddr = inet:ntoa(RAddr),
    {Host, _} = cowboy_req:header(<<"host">>, Req, <<"-">>),
    {Method, _} = cowboy_req:method(Req),
    {Path, _} = cowboy_req:path(Req),
    {UserAgent, _} = cowboy_req:header(<<"user-agent">>, Req, <<"-">>),
    error_logger:info_msg("[sheep2] ~s ~s - \"~s ~s\" ~w ~s",
        [
            RemoteAddr, Host,
            Method, Path,
            StatusCode, UserAgent
        ]).
