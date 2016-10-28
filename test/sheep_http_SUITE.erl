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
    F = fun(URL) ->
        {ok, 200, _, Ref} = hackney:request(get, URL, ?HEADERS),
        {ok, Body} = hackney:body(Ref),
        jiffy:decode(Body, [return_maps])
        end,
    #{
        <<"reply_from">> := <<"read">>
    } = F(build_url("/simple")),
    #{
        <<"got_page">> := <<"25">>,
        <<"got_order">> := <<"asc">>
    } = F(build_url("/simple?page=25&order=asc")),
    ok.


post_put_delete_test(_Config) ->
    URL = build_url("/simple"),
    F = fun(Method) ->
        {ok, 200, _, Ref} = hackney:request(Method, URL, ?HEADERS),
        {ok, Body} = hackney:body(Ref),
        jiffy:decode(Body, [return_maps])
        end,
    #{<<"reply_from">> := <<"create">>} = F(post),
    #{<<"reply_from">> := <<"update">>} = F(put),
    #{<<"reply_from">> := <<"delete">>} = F(delete),
    ok.


pipeline_test(_Config) ->
    URL = build_url("/pipeline/users"),
    H = [{<<"x-auth-token">>, <<"cft6GLEhLANgstU8sZdL">>} | ?HEADERS],
    {ok, 200, _, Ref1} = hackney:request(get, URL, H),
    {ok, Body1} = hackney:body(Ref1),
    #{
        <<"reply_from">> := <<"read">>,
        <<"steps">> := [<<"paging">>, <<"auth">>]
    } = jiffy:decode(Body1, [return_maps]),

    {ok, 401, _, Ref2} = hackney:request(get, URL, ?HEADERS),
    {ok, <<"Auth error">>} = hackney:body(Ref2),

    {ok, 200, _, Ref3} = hackney:request(post, URL, H, <<"{\"user_id\":25}">>),
    {ok, Body3} = hackney:body(Ref3),
    #{
        <<"reply_from">> := <<"create">>,
        <<"user_id">> := 25,
        <<"steps">> := [<<"validation">>, <<"auth">>]
    } = jiffy:decode(Body3, [return_maps]),

    {ok, 400, _, Ref4} = hackney:request(post, URL, H, <<"{\"id\":25}">>),
    {ok, Body4} = hackney:body(Ref4),
    #{
        <<"error">> := <<"User ID not provided">>
    } = jiffy:decode(Body4, [return_maps]),

    ok.


status_test(_Config) ->
    URL0 = build_url("/status/users"),
    {ok, 200, _, Ref0} = hackney:request(get, URL0, ?HEADERS),
    {ok, Body0} = hackney:body(Ref0),
    [
        #{<<"id">> := <<"1">>, <<"name">> := <<"Username 1">>},
        #{<<"id">> := <<"2">>, <<"name">> := <<"Username 2">>}
    ] = jiffy:decode(Body0, [return_maps]),

    URL1 = build_url("/status/users/1"),
    {ok, 200, _, Ref1} = hackney:request(get, URL1, ?HEADERS),
    {ok, Body1} = hackney:body(Ref1),
    #{<<"id">> := <<"1">>, <<"name">> := <<"Username 1">>}
        = jiffy:decode(Body1, [return_maps]),

    URL2 = build_url("/status/users/2"),
    {ok, 204, Headers2, Ref2} = hackney:request(get, URL2, ?HEADERS),
    <<"0">> = proplists:get_value(<<"content-length">>, Headers2),
    {ok, <<>>} = hackney:body(Ref2),

    URL3 = build_url("/status/users/3"),
    {ok, 204, Headers3, Ref3} = hackney:request(get, URL3, ?HEADERS),
    <<"0">> = proplists:get_value(<<"content-length">>, Headers3),
    {ok, <<>>} = hackney:body(Ref3),

    URL4 = build_url("/status/users/4"),
    {ok, 404, _, Ref4} = hackney:request(get, URL4, ?HEADERS),
    {ok, <<"Not found">>} = hackney:body(Ref4),

    ok.


error_status_test(_Config) ->
    URL1 = build_url("/e/status/users"),
    {ok, 200, _, _} = hackney:request(get, URL1, ?HEADERS),

    URL2 = build_url("/e/status/users/2"),
    {ok, 404, _, _} = hackney:request(get, URL2, ?HEADERS),

    URL3 = build_url("/e/status/users/3"),
    {ok, 400, _, Ref3} = hackney:request(get, URL3, ?HEADERS),
    {ok, Body3} = hackney:body(Ref3),
    #{
        <<"error">> := <<"simple_error">>
    } = jiffy:decode(Body3, [return_maps]),

    URL4 = build_url("/e/status/users/4"),
    {ok, 500, _, Ref4} = hackney:request(get, URL4, ?HEADERS),
    {ok, Body4} = hackney:body(Ref4),
    #{
        <<"error">> := <<"custom_error">>
    } = jiffy:decode(Body4, [return_maps]),
    
    URL5 = build_url("/e/status/users/5"),
    {ok, 400, _, Ref5} = hackney:request(get, URL5, ?HEADERS),
    {ok, Body5} = hackney:body(Ref5),
    #{
        <<"error">> := <<"Test exception">>
    } = jiffy:decode(Body5, [return_maps]),
    ok.


encode_decode_test(_Config) ->
    URL1 = build_url("/encode_decode"),
    Data = #{<<"answer">> => 42},

    JData = jiffy:encode(Data),
    {ok, 200, _, Ref} = hackney:request(get, URL1, ?HEADERS, JData),
    {ok, Body} = hackney:body(Ref),
    #{
        <<"answer">> := 42,
        <<"custom_decoder">> := <<"ok">>,
        <<"readed">> := true,
        <<"custom_encoder">> := <<"ok">>
    } = jiffy:decode(Body, [return_maps]),

    MData = msgpack:pack(Data),
    {ok, 415, _, _} = hackney:request(get, URL1, ?M_HEADERS, MData),

    URL2 = build_url("/encode_decode/empty"),
    {ok, 204, _, _} = hackney:request(get, URL2, ?HEADERS),

    URL3 = build_url("/encode_decode/empty_404"),
    {ok, 404, _, _} = hackney:request(get, URL3, ?HEADERS),

    URL4 = build_url("/encode_decode/undefined"),
    {ok, 204, _, _} = hackney:request(get, URL4, ?HEADERS),
    ok.


invalid_handler_test(_Config) ->
    URL = build_url("/invalid"),
    {ok, 204, _, _} = hackney:request(get, URL, ?HEADERS), % empty list of callbacks
    {ok, 405, _, _} = hackney:request(put, URL, ?HEADERS), % no callbacks in methods_spec
    {ok, 501, _, _} = hackney:request(post, URL, ?HEADERS), % callback in list but not exported
    ok.


%%% Utils

build_url(Path) ->
    Port = integer_to_list(ranch:get_port(sheep_test_server)),
    "http://localhost:" ++ Port ++ Path.


