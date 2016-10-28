-module(sheep_http_SUITE).

-include_lib("common_test/include/ct.hrl").

-compile([export_all]).

-define(HEADERS, [
    {<<"content-type">>, <<"application/json">>},
    {<<"accept">>, <<"application/json">>}
]).

-define(M_HEADERS, [
    {<<"content-type">>, <<"application/x-msgpack">>},
    {<<"accept">>, <<"application/x-msgpack">>}
]).

all() ->
    [
        get_test,
        post_put_delete_test,
        pipeline_test,
        status_test,
        error_status_test,
        encode_decode_test,
        invalid_handler_test
    ].


%%% Init

init_per_suite(Config) ->
    application:ensure_all_started(cowboy),
    hackney:start(),

    Routing = cowboy_router:compile([
        {"localhost", [
            {"/simple[/:param]", simple_handler, []},
            {"/pipeline/users[/:user_id]", pipeline_handler, []},
            {"/status/users[/:user_id]", status_samples_handler, []},
            {"/e/status/users[/:user_id]", error_status_samples_handler, []},
            {"/encode_decode[/:kind]", encode_decode_handler, []},
            {"/invalid", invalid_handler, []}
        ]}
    ]),

    {ok, _} = cowboy:start_http(sheep_test_server, 100, [{port, 0}],
        [
            {env, [{dispatch, Routing}]},
            {max_keepalive, 50},
            {timeout, 500}
        ]),
    Config.


end_per_suite(_Config) ->
    cowboy:stop_listener(sheep_test_server),
    ok.


%%% Tests

get_test(_Config) ->
    {200, _, Body1} = query("/simple"),
    #{<<"reply_from">> := <<"read">>} = jiffy:decode(Body1, [return_maps]),

    {200, _, Body2} = query("/simple?page=25&order=asc"),
    #{
        <<"got_page">> := <<"25">>,
        <<"got_order">> := <<"asc">>
    } = jiffy:decode(Body2, [return_maps]),
    ok.


post_put_delete_test(_Config) ->
    {200, _, Body1} = query(post, "/simple"),
    #{<<"reply_from">> := <<"create">>} = jiffy:decode(Body1, [return_maps]),

    {200, _, Body2} = query(put, "/simple"),
    #{<<"reply_from">> := <<"update">>} = jiffy:decode(Body2, [return_maps]),

    {200, _, Body3} = query(delete, "/simple"),
    #{<<"reply_from">> := <<"delete">>} = jiffy:decode(Body3, [return_maps]),
    ok.


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
        <<"steps">> := [<<"validation">>, <<"auth">>]
    } = jiffy:decode(Body3, [return_maps]),

    {400, _, Body4} = query(post, URL, H, <<"{\"id\":25}">>),
    #{<<"error">> := <<"User ID not provided">>} = jiffy:decode(Body4, [return_maps]),
    ok.


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
    <<"0">> = proplists:get_value(<<"content-length">>, Headers2),

    {204, Headers3, <<>>} = query("/status/users/3"),
    <<"0">> = proplists:get_value(<<"content-length">>, Headers3),

    {404, _, <<"Not found">>} = query("/status/users/4"),
    ok.


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


invalid_handler_test(_Config) ->
    Path = "/invalid",
    {204, _, <<>>} = query(get, Path), % empty list of callbacks
    {405, _, <<"Method not allowed">>} = query(put, Path), % no callbacks in methods_spec
    {501, _, <<"Not implemented">>} = query(post, Path), % callback in list but not exported
    ok.


%%% Utils

query(Path) ->
    query(get, Path).

query(Method, Path) ->
    query(Method, Path, ?HEADERS).

query(Method, Path, Headers) ->
    query(Method, Path, Headers, <<>>).

query(Method, Path, Headers, Data) ->
    Port = integer_to_list(ranch:get_port(sheep_test_server)),
    FullURL = "http://localhost:" ++ Port ++ Path,
    {ok, Status, RHeaders, Ref} = hackney:request(Method, FullURL, Headers, Data),
    {ok, Body} = hackney:body(Ref),
    {Status, RHeaders, Body}.
