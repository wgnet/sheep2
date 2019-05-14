-module(sheep_http).
-behaviour(cowboy_sub_protocol).

-export([upgrade/4, upgrade/5, get_header/2, get_header/3]).

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

-spec upgrade(cowboy_req:req(), cowboy_middleware:env(), module(), any(), any())
	-> {ok, Req, Env} when Req::cowboy_req:req(), Env::cowboy_middleware:env().
%% cowboy_rest takes no options.
upgrade(Req, Env, Handler, _HandlerState, Opts) ->
	upgrade(Req, Env, Handler, Opts).

-spec upgrade(cowboy_req:req(), cowboy_middleware:env(), module(), term()) ->
                     {ok, cowboy_req:req(), cowboy_middleware:env()}.
upgrade(CowRequest0, Env, Handler, HandlerOpts) ->
    Method = cowboy_req:method(CowRequest0),
    Headers = cowboy_req:headers(CowRequest0),
    Bindings = cowboy_req:bindings(CowRequest0),
    Query = cowboy_req:parse_qs(CowRequest0),
    Peer = cowboy_req:peer(CowRequest0),
    {Body, CowRequest} = case cowboy_req:has_body(CowRequest0) of
        true ->
            {ok, Body0, CowRequest1} = read_body(CowRequest0),
            {Body0, CowRequest1};
        false ->
            {<<>>, CowRequest0}
    end,
    Request = #sheep_request{
        method = Method,
        headers = Headers,
        bindings = Bindings,
        query = to_map(Query),
        body = Body,
        peer = Peer
    },

    {Options, Response} =
        try
            {Options0, State} =
                case erlang:function_exported(Handler, sheep_init, 2) of
                    true -> Handler:sheep_init(Request, HandlerOpts);
                    false -> {#sheep_options{}, []}
                end,
            case decode_payload(Handler, Request, Options0) of
                {ok, Request2} ->
                    Response0 = handle(Request2, Handler, Options0, State),
                    {Options0, Response0};
                {error, Response0} -> {Options0, Response0}
            end
        catch
            Class:Reason ->
                {#sheep_options{}, handle_exception(Handler, Request, Class, Reason)}
        end,
    Response2 = encode_payload(Handler, Request, Response, Options),
    #sheep_response{
        status_code = ResponseCode,
        headers = ResponseHeaders,
        body = ResponseBody
    } = Response2,
    log_query(CowRequest, Request, Response2),

    CowReply = cowboy_req:reply(ResponseCode, ResponseHeaders, ResponseBody, CowRequest),
    {ok, CowReply, Env}.


-spec get_header(binary(), #sheep_request{}) -> binary() | undefined.
get_header(Name, Request) ->
    get_header(Name, Request, undefined).

-spec get_header(binary(), #sheep_request{}, binary() | undefined) -> binary() | undefined.
get_header(Name, #sheep_request{headers = Headers}, Default) ->
    maps:get(Name, Headers, Default).


%%% Inner functions

-spec decode_payload(module(), #sheep_request{}, #sheep_options{}) ->
                            {ok, #sheep_request{}} | {error, #sheep_response{}}.
decode_payload(_, #sheep_request{body = <<>>} = Request, _Options) ->
    {ok, Request};
decode_payload(Handler, #sheep_request{body = Body} = Request, Options) ->
    RawContentType = get_header(<<"content-type">>, Request),
    CleanContentType = clean_content_type(RawContentType),
    DecodeSpec = decode_spec(Options),
    DefaultDec = maps:find(undefined, DecodeSpec),
    DefinedDec = maps:find(CleanContentType, DecodeSpec),

    case get_decoder(DefinedDec, DefaultDec) of
        error ->
            error_logger:info_report([
                {error, "not supported"},
                {<<"content-type">>, RawContentType}
            ]),
            {error, handle_internal_error(
                      Handler, Request, {unsupported_content_type, RawContentType},
                      #sheep_response{status_code = 415, body = <<"Not supported 'content-type'">>})};
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
                    {error, handle_internal_error(
                              Handler, Request, {request_decode_error, Class, Reason, RawContentType},
                              #sheep_response{status_code = 400, body = E})}
            end
    end.

-spec encode_payload(module(), #sheep_request{}, #sheep_response{}, #sheep_options{}) -> #sheep_response{}.
encode_payload(_Handler, _Request, #sheep_response{body = Body} = Response, _Options) when is_binary(Body) ->
    Response;
encode_payload(_Handler, _Request, #sheep_response{status_code = 204} = Response, _Options) ->
    Response#sheep_response{body = <<>>};
encode_payload(_Handler, _Request, #sheep_response{body = undefined} = Response, _Options) ->
    Response#sheep_response{body = <<>>};
encode_payload(Handler, Request, #sheep_response{body = Body, headers = Headers} = Response, Options) ->
    AcceptContentType =
        case get_header(<<"accept">>, Request) of
            undefined -> get_header(<<"content-type">>, Request);
            A -> A
        end,
    CleanAcceptContentType = clean_accept_content_type(AcceptContentType),
    CleanAcceptContentTypeKeys = maps:keys(CleanAcceptContentType),
    EncodeSpec = encode_spec(Options),
    MapIntersection = maps:with(CleanAcceptContentTypeKeys, EncodeSpec),
    DefaultEnc = maps:find(undefined, EncodeSpec),

    case sort_by_quality(maps:to_list(MapIntersection), DefaultEnc) of
        [] ->
            error_logger:info_report([
                {error, "not acceptable"},
                {<<"content-type">>, AcceptContentType}
            ]),
            handle_internal_error(
                Handler, Request, {unsupported_accept, AcceptContentType},
                sheep_response:new_406()
            );
        [{AcceptContentTypeChoosen, Fn} | _] ->
            try
                Headers2 = maps:put(<<"content-type">>, AcceptContentTypeChoosen, Headers),
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
                    handle_internal_error(
                        Handler, Request, {response_encode_error, Class, Reason, AcceptContentType},
                        #sheep_response{status_code = 500, body = E}
                    )
            end
    end.

-spec handle(#sheep_request{}, module(), #sheep_options{}, term()) -> #sheep_response{}.
handle(#sheep_request{method = Method} = Request, HandlerModule, Options, State) ->
    MethodsSpec = method_spec(Options),
    case maps:find(Method, MethodsSpec) of
        {ok, Handlers} when is_list(Handlers) ->
            call_handlers(Handlers, Request, HandlerModule, State);
        error ->
            handle_internal_error(HandlerModule, Request, method_not_allowed, sheep_response:new_405())
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
                _ ->
                    handle_internal_error(
                      Module, Request, {handler_callback_missing, Module, Handler},
                      sheep_response:new_501())
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


handle_internal_error(Handler, Request, Reason, Default) ->
    case erlang:function_exported(Handler, exception_handler, 3) of
        true ->
            try
                Handler:exception_handler(Request, sheep_internal_error, Reason)
            catch
                Class1:Reason1 ->
                    ST1 = erlang:get_stacktrace(),
                    error_logger:error_report([
                        {error, invalid_exception_handler},
                        {handler, Handler},
                        {exception, {Class1, Reason1}},
                        {stacktrace, ST1}
                    ]),
                    Default
            end;
        false ->
            Default
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


to_num(Bin) when is_binary(Bin) ->
    to_num(binary_to_list(Bin));
to_num(Lst) when is_list(Lst) ->
    case string:to_float(Lst) of
        {error,no_float} -> list_to_integer(Lst);
        {F,_Rest} -> F
    end;
to_num(Int) when is_integer(Int) -> Int;
to_num(Float) when is_float(Float) -> Float;
to_num(_) -> 0.

get_decoder(error, error) -> error;
get_decoder({ok, ClientDefinedFn}, _) -> {ok, ClientDefinedFn};
get_decoder(_, {ok, DefaultFn}) -> {ok, DefaultFn}.

parse_quality(Q) ->
    try
        to_num(Q)
    catch error:badarg ->
        0
    end.

sort_by_quality([], error) -> [];
sort_by_quality([], {ok, DefaultEnc}) -> [{<<"*/*">>, DefaultEnc}];
sort_by_quality(AcceptTypes, _) ->
    lists:sort(
        fun ({_, Opt1P}, {_, Opt2P}) ->
            Q1 = proplists:get_value(<<"q">>, Opt1P, 1),
            Q2 = proplists:get_value(<<"q">>, Opt2P, 1),
            parse_quality(Q1) > parse_quality(Q2)
        end, AcceptTypes).

clean_accept_content_type(undefined) -> #{};
clean_accept_content_type(BinAccept) when is_binary(BinAccept) ->
    Stripped = binary:replace(BinAccept, <<" ">>, <<>>, [global]),
    lists:foldl(fun parse_accept_option/2, #{}, binary:split(Stripped, <<",">>, [global])).

parse_accept_option(OptionBin, Acc) ->
    [Option | RawParams] = binary:split(OptionBin, <<";">>, [global]),
    case Option of
        <<>> -> Acc;
        _ ->
            Params = lists:filtermap(fun parse_accept_param/1, RawParams),
            Acc#{Option => Params}
    end.

parse_accept_param(Param) ->
    case binary:split(Param, <<"=">>, [global]) of
        [Name, Value] -> {true, {Name, Value}};
        _ -> false
    end.

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

-spec read_body(cowboy_req:req()) -> {binary(), cowboy_req:req()}.
read_body(Req0) ->
    case cowboy_req:has_body(Req0) of
        true ->
            read_body(Req0, <<>>);
        false ->
            {<<>>, Req0}
    end.

read_body(Req0, Acc) ->
    case cowboy_req:read_body(Req0) of
        {ok, Data, Req} ->
            {<<Acc/binary, Data/binary>>, Req};
        {more, Data, Req} ->
            read_body(Req, <<Acc/binary, Data/binary>>)
    end.

-spec to_binary(atom() | list() | binary()) -> binary().
to_binary(V) when is_atom(V) -> to_binary(atom_to_list(V));
to_binary(V) when is_list(V) -> list_to_binary(V);
to_binary(V) when is_binary(V) -> V.


-spec log_query(cowboy_req:req(), #sheep_request{}, #sheep_response{}) -> ok.
log_query(CowRequest, Request, Response) ->
    case application:get_env(sheep2, log_callback) of
        {ok, Fun} when is_function(Fun) -> Fun({CowRequest, Request, Response});
        {ok, undefined} -> ok;
        undefined -> ok
    end.
