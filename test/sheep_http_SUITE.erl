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
        basic_users_handler_test,
        basic_user_handler_with_error_handler_test,
        users_handler_with_transitions_test,
        encode_decode_handler_test,
        status_204_test
    ].

init_dispatch() ->
    cowboy_router:compile([
        {"localhost", [
            {"/simple[/:param]", simple_handler, []},
            {"/basic/users[/:user_id]", basic_users_handler, []},
            {"/basic_with_error_handler/users[/:user_id]", basic_users_handler_with_error_handler, []},
            {"/transitions/users[/:user_id]", users_handler_with_transitions, []},
            {"/full/users[/:user_id]", full_users_handler, []},
            {"/customers[/:customer_id]/orders[/:order_id]", orders_handler, []},
            {"/encode_decode[/:kind]", encode_decode_handler, []}
        ]}
    ]).

init_per_suite(Config) ->
    ct:log("~p~n", [Config]),
    application:ensure_all_started(cowboy),
    {ok, _} = cowboy:start_http(sheep_test_server, 100, [{port, 0}],
        [
            {env, [{dispatch, init_dispatch()}]},
            {max_keepalive, 50},
            {timeout, 500}
        ]),
    Port = ranch:get_port(sheep_test_server),
    hackney:start(),
    [{port, Port} | Config].


end_per_suite(_Config) ->
    cowboy:stop_listener(sheep_test_server),
    ok.


build_url(Path, Config) ->
    Port = ?config(port, Config),
    PortBin = list_to_binary(integer_to_list(Port)),
    <<"http://localhost:", PortBin/binary, Path/binary >>.


basic_users_handler_test(Config) ->
    URL1 = build_url(<<"/basic/users">>, Config),
    {ok, 200, _, _} = hackney:request(get, URL1, ?HEADERS),

    URL2 = build_url(<<"/basic/users/1">>, Config),
    {ok, 404, _, _} = hackney:request(get, URL2, ?HEADERS),
    ok.


basic_user_handler_with_error_handler_test(Config) ->
    URL1 = build_url(<<"/basic_with_error_handler/users">>, Config),
    {ok, 200, _, _} = hackney:request(get, URL1, ?HEADERS),

    URL2 = build_url(<<"/basic_with_error_handler/users/1">>, Config),
    {ok, 404, _, _} = hackney:request(get, URL2, ?HEADERS),

    URL3 = build_url(<<"/basic_with_error_handler/users/error_id">>, Config),
    {ok, 400, _, _} = hackney:request(get, URL3, ?HEADERS),

    URL4 = build_url(<<"/basic_with_error_handler/users/custom_error_id">>, Config),
    {ok, 400, _, _} = hackney:request(get, URL4, ?HEADERS),

    URL5 = build_url(<<"/basic_with_error_handler/users/throw_id">>, Config),
    {ok, 400, _, _} = hackney:request(get, URL5, ?HEADERS),
    ok.


users_handler_with_transitions_test(Config) ->
    URL1 = build_url(<<"/transitions/users">>, Config),
    {ok, 200, _, _} = hackney:request(get, URL1, ?HEADERS),
    ok.


encode_decode_handler_test(Config) ->
    URL1 = build_url(<<"/encode_decode">>, Config),
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

    URL2 = build_url(<<"/encode_decode/empty">>, Config),
    {ok, 204, _, _} = hackney:request(get, URL2, ?HEADERS),

    URL3 = build_url(<<"/encode_decode/empty_404">>, Config),
    {ok, 404, _, _} = hackney:request(get, URL3, ?HEADERS),

    URL4 = build_url(<<"/encode_decode/undefined">>, Config),
    {ok, 204, _, _} = hackney:request(get, URL4, ?HEADERS),
    ok.


status_204_test(Config) ->
    URL1 = build_url(<<"/simple/empty_1">>, Config),
    {ok, 204, Headers1, Ref1} = hackney:request(get, URL1, ?HEADERS),
    <<"0">> = proplists:get_value(<<"content-length">>, Headers1),
    {ok, <<>>} = hackney:body(Ref1),

    URL2 = build_url(<<"/simple/empty_2">>, Config),
    {ok, 204, Headers2, Ref2} = hackney:request(get, URL2, ?HEADERS),
    <<"0">> = proplists:get_value(<<"content-length">>, Headers2),
    {ok, <<>>} = hackney:body(Ref2),
    ok.


