-module(sheep_http_SUITE).

-include_lib("common_test/include/ct.hrl").

-compile([export_all]).

-include("sheep.hrl").

-define(HEADERS, [
    {<<"content-type">>, <<"application/json">>},
    {<<"accept">>, <<"application/json">>}
]).

-define(M_HEADERS, [
    {<<"content-type">>, <<"application/x-msgpack">>},
    {<<"accept">>, <<"application/x-msgpack">>}
]).


-spec all() -> [atom()].
all() ->
    [
        ping_test,
        get_test,
        post_put_delete_test,
        pipeline_test,
        status_test,
        error_status_test,
        error_with_stacktrace_test,
        encode_decode_test,
        invalid_handler_test,
        invalid_encode_decode_test,
        invalid_headers_test,
        internal_errors_test
    ].


%%% Init

-spec init_per_suite(list()) -> list().
init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(cowboy),
    {ok, _} = application:ensure_all_started(hackney),

    Routing = cowboy_router:compile([
        {"localhost", [
            {"/ping", ping_handler, []},
            {"/simple[/:param]", simple_handler, []},
            {"/pipeline/users[/:user_id]", pipeline_handler, []},
            {"/status/users[/:user_id]", status_samples_handler, []},
            {"/e/status/users[/:user_id]", error_status_samples_handler, []},
            {"/est/status/users[/:user_id]", error_with_stacktrace_handler, []},
            {"/encode_decode[/:kind]", encode_decode_handler, []},
            {"/invalid", invalid_handler, []},
            {"/invalid/init", invalid_init_handler, []},
            {"/invalid/init/2", invalid_init_2_handler, []},
            {"/invalid/init/3", invalid_init_3_handler, []},
            {"/internal_error/:type", internal_error_handler, []}
        ]}
    ]),

    {ok, _} = cowboy:start_clear(sheep_test_server, [{port, 0}],
        #{
            env => #{dispatch => Routing},
            max_keepalive => 50,
            timeout => 500
        }),
    Config.


-spec end_per_suite(list()) -> ok.
end_per_suite(_Config) ->
    cowboy:stop_listener(sheep_test_server),
    ok.


%%% Tests

-spec ping_test(list()) -> ok.
ping_test(_Config) ->
    {200, _, <<"pong">>} = query("/ping"),
    ok.


-spec get_test(list()) -> ok.
get_test(_Config) ->
    {200, _, Body1} = query("/simple"),
    #{
        <<"path">> := <<"/simple">>,
        <<"reply_from">> := <<"read">>
    } = jiffy:decode(Body1, [return_maps]),

    {200, _, Body2} = query("/simple?page=25&order=asc"),
    #{
        <<"path">> := <<"/simple">>,
        <<"got_page">> := <<"25">>,
        <<"got_order">> := <<"asc">>
    } = jiffy:decode(Body2, [return_maps]),
    ok.


-spec post_put_delete_test(list()) -> ok.
post_put_delete_test(_Config) ->
    {200, _, Body1} = query(post, "/simple"),
    #{<<"reply_from">> := <<"create">>} = jiffy:decode(Body1, [return_maps]),

    {200, _, Body2} = query(put, "/simple"),
    #{<<"reply_from">> := <<"update">>} = jiffy:decode(Body2, [return_maps]),

    {200, _, Body3} = query(delete, "/simple"),
    #{<<"reply_from">> := <<"delete">>} = jiffy:decode(Body3, [return_maps]),
    ok.


-spec pipeline_test(list()) -> ok.
pipeline_test(_Config) ->
    URL = "/pipeline/users",
    H = [{<<"x-auth-token">>, <<"cft6GLEhLANgstU8sZdL">>} | ?HEADERS],
    {200, _, Body1} = query(get, URL, H),
    #{
        <<"reply_from">> := <<"read">>,
        <<"steps">> := [<<"paging">>, <<"auth">>]
    } = jiffy:decode(Body1, [return_maps]),

    {401, _, <<"Auth error">>} = query(URL),

    {200, _, Body3} = query(post, URL, H, <<"{\"user_id\":25}">>),
    #{
        <<"reply_from">> := <<"create">>,
        <<"user_id">> := 25,
        <<"steps">> := [<<"stage4">>, <<"stage3">>, <<"validation">>, <<"auth">>]
    } = jiffy:decode(Body3, [return_maps]),

    {400, _, Body4} = query(post, URL, H, <<"{\"id\":25}">>),
    #{<<"error">> := <<"User ID not provided">>} = jiffy:decode(Body4, [return_maps]),
    ok.


-spec status_test(list()) -> ok.
status_test(_Config) ->
    {200, _, Body0} = query("/status/users"),
    [
        #{<<"id">> := <<"1">>, <<"name">> := <<"Username 1">>},
        #{<<"id">> := <<"2">>, <<"name">> := <<"Username 2">>}
    ] = jiffy:decode(Body0, [return_maps]),

    {200, _, Body1} = query("/status/users/1"),
    #{<<"id">> := <<"1">>, <<"name">> := <<"Username 1">>}
        = jiffy:decode(Body1, [return_maps]),

    {204, Headers2, <<>>} = query("/status/users/2"),
    undefined = proplists:get_value(<<"content-length">>, Headers2),

    {204, Headers3, <<>>} = query("/status/users/3"),
    undefined = proplists:get_value(<<"content-length">>, Headers3),

    {404, _, <<"Not found">>} = query("/status/users/4"),
    ok.


-spec error_status_test(list()) -> ok.
error_status_test(_Config) ->
    {200, _, _} = query("/e/status/users"),
    {404, _, _} = query("/e/status/users/2"),

    {400, _, Body3} = query("/e/status/users/3"),
    #{<<"error">> := <<"simple_error">>} = jiffy:decode(Body3, [return_maps]),

    {500, _, Body4} = query("/e/status/users/4"),
    #{<<"error">> := <<"custom_error">>} = jiffy:decode(Body4, [return_maps]),

    {400, _, Body5} = query("/e/status/users/5"),
    #{<<"error">> := <<"Test exception">>} = jiffy:decode(Body5, [return_maps]),

    {500, _, <<"Internal server error">>} = query("/status/users/5"),
    ok.


-spec error_with_stacktrace_test(list()) -> ok.
error_with_stacktrace_test(_Config) ->
    {200, _, _} = query("/est/status/users"),
    {500, _, Body5} = query("/est/status/users/5"),
    #{
      <<"error">> := <<"Test exception">>,
      <<"stacktrace">> := _
     } = jiffy:decode(Body5, [return_maps]),
    ok.


-spec encode_decode_test(list()) -> ok.
encode_decode_test(_Config) ->
    Data = #{<<"answer">> => 42},
    JData = jiffy:encode(Data),
    {200, _, Body} = query(get, "/encode_decode", ?HEADERS, JData),
    #{
        <<"answer">> := 42,
        <<"custom_decoder">> := <<"ok">>,
        <<"readed">> := true,
        <<"custom_encoder">> := <<"ok">>
    } = jiffy:decode(Body, [return_maps]),

    MData = msgpack:pack(Data),
    {415, _, <<"Not supported 'content-type'">>} = query(get, "/encode_decode", ?M_HEADERS, MData),

    {204, _, _} = query("/encode_decode/empty"),
    {404, _, _} = query("/encode_decode/empty_404"),
    {204, _, _} = query("/encode_decode/undefined"),
    ok.


-spec invalid_handler_test(list()) -> ok.
invalid_handler_test(_Config) ->
    Path = "/invalid",
    {204, _, <<>>} = query(get, Path), % empty list of callbacks
    {405, _, <<"Method not allowed">>} = query(put, Path), % no callbacks in methods_spec
    {501, _, <<"Not implemented">>} = query(post, Path), % callback in list but not exported

    {500, _, <<"Internal server error">>} = query("/invalid/init"),
    {400, _, Body1} = query("/invalid/init/2"),
    #{<<"error">> := <<"Test exception">>} = jiffy:decode(Body1, [return_maps]),
    {500, _, <<"Internal server error">>} = query("/invalid/init/3"),
    ok.


-spec invalid_encode_decode_test(list()) -> ok.
invalid_encode_decode_test(_Config) ->
    Data = <<"{answer\":42}">>,
    {400, _, <<"Can't decode 'application/json' payload">>} =
        query(get, "/encode_decode", ?HEADERS, Data),
    {500, _, <<"Can't encode 'application/json' payload">>} =
        query("/encode_decode/invalid_payload"),
    ok.


-spec invalid_headers_test(list()) -> ok.
invalid_headers_test(_Config) ->
    Data = #{<<"answer">> => 42},
    JData = jiffy:encode(Data),
    MData = msgpack:pack(Data),
    {415, _, <<"Not supported 'content-type'">>} = query(get, "/simple", [], JData),
    {415, _, <<"Not supported 'content-type'">>} =
        query(get, "/simple", [{<<"content-type">>, <<"text/html">>}], JData),
    {200, _, _} = %% skipping "accept" header is possible
        query(get, "/simple", [{<<"content-type">>, <<"application/x-msgpack">>}], MData),
    {406, _, <<"Not acceptable">>} =
        query(get, "/simple", [
            {<<"content-type">>, <<"application/x-msgpack">>},
            {<<"accept">>, <<"text/html">>}
        ], MData),
    {200, _, _} =
        query(get, "/simple", [
            {<<"content-type">>, <<"application/x-msgpack">>},
            {<<"accept">>, <<"application/x-msgpack">>}
        ], MData),
    ok.

internal_errors_test(_Config) ->
    {400, _, ResBody1} =
        query(post, "/internal_error/request_decode_error",
          [{<<"content-type">>, <<"application/json">>}],
          <<"{">>),
    #{<<"type">> := <<"request_decode_error">>,
      <<"content_type">> := <<"application/json">>} = jiffy:decode(ResBody1, [return_maps]),

    {415, _, ResBody2} =
        query(post, "/internal_error/request_decode_error",
          [{<<"content-type">>, <<"text/plain">>}],
          <<"wasd">>),
    #{<<"type">> := <<"unsupported_content_type">>,
      <<"content_type">> := <<"text/plain">>} = jiffy:decode(ResBody2, [return_maps]),

    {500, _, ResBody3} =
        query(post, "/internal_error/response_encode_error",
          [{<<"content-type">>, <<"application/json">>},
           {<<"accept">>, <<"application/json">>}],
          <<"{}">>),
    #{<<"type">> := <<"response_encode_error">>,
      <<"content_type">> := <<"application/json">>} = jiffy:decode(ResBody3, [return_maps]),

    {406, _, ResBody4} =
        query(post, "/internal_error/unsupported_accept",
          [{<<"content-type">>, <<"application/json">>},
           {<<"accept">>, <<"text/plain">>}],
          <<"{}">>),
    #{<<"type">> := <<"unsupported_accept">>,
      <<"content_type">> := <<"text/plain">>} = jiffy:decode(ResBody4, [return_maps]),

    {405, _, ResBody5} =
        query(patch, "/internal_error/method_not_allowed",
          [{<<"content-type">>, <<"application/json">>},
           {<<"accept">>, <<"application/json">>}],
          <<"{}">>),
    #{<<"type">> := <<"method_not_allowed">>} = jiffy:decode(ResBody5, [return_maps]),

    {501, _, ResBody6} =
        query(get, "/internal_error/handler_callback_missing",
          [{<<"content-type">>, <<"application/json">>},
           {<<"accept">>, <<"application/json">>}],
          <<"{}">>),
    #{<<"type">> := <<"handler_callback_missing">>,
      <<"module">> := <<"internal_error_handler">>,
      <<"handler">> := <<"read">>} = jiffy:decode(ResBody6, [return_maps]),
    ok.

%%% Utils

-spec query(string()) -> {http_code(), list(), binary()}.
query(Path) ->
    query(get, Path).

-spec query(atom(), string()) -> {http_code(), list(), binary()}.
query(Method, Path) ->
    query(Method, Path, ?HEADERS).

-spec query(atom(), string(), list()) -> {http_code(), list(), binary()}.
query(Method, Path, Headers) ->
    query(Method, Path, Headers, <<>>).

-spec query(atom(), string(), list(), binary()) -> {http_code(), list(), binary()}.
query(Method, Path, Headers, Data) ->
    Port = integer_to_list(ranch:get_port(sheep_test_server)),
    FullURL = "http://localhost:" ++ Port ++ Path,
    {ok, Status, RHeaders, Ref} = hackney:request(Method, FullURL, Headers, Data),
    {ok, Body} = hackney:body(Ref),
    {Status, RHeaders, Body}.
