-module(sheep_http_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([
    all/0,
    init_per_suite/1,
    end_per_suite/1
]).

-export([
    basic_users_handler_test/1,
    basic_user_handler_with_error_handler_test/1,
    users_handler_with_transitions_test/1,
    encode_decode_handler_test/1
]).

all() ->
    [
        basic_users_handler_test,
        basic_user_handler_with_error_handler_test,
        users_handler_with_transitions_test,
        encode_decode_handler_test
    ].

init_dispatch() ->
    cowboy_router:compile([
        {"localhost", [
            {
                "/basic/users[/:user_id]",
                basic_users_handler, []},
            {
                "/basic_with_error_handler/users[/:user_id]",
                basic_users_handler_with_error_handler, []
            },
            {
                "/transitions/users[/:user_id]",
                users_handler_with_transitions, []
            },
            {
                "/full/users[/:user_id]",
                full_users_handler, []},
            {
                "/customers[/:customer_id]/orders[/:order_id]",
                orders_handler, []},
            {
                "/encode_decode",
                encode_decode_handler, []}
        ]}
    ]).

init_per_suite(Config) ->
    io:format("~p~n", [Config]),
    application:ensure_all_started(cowboy),
    {ok, _} = cowboy:start_http(
        server, 100, [{port, 0}], [
            {env, [
                {dispatch, init_dispatch()}
            ]},
            {max_keepalive, 50},
            {timeout, 500}
    ]),
    Port = ranch:get_port(server),
    {ok, Client} = cowboy_client:init([]),
    [
        {port, Port},
        {client, Client}
        |Config
    ].


end_per_suite(_Config) ->
    cowboy:stop_listener(server),
    ok.


build_url(Path, Config) ->
    Port = ?config(port, Config),
    PortBin = list_to_binary(integer_to_list(Port)),
    <<"http://localhost:", PortBin/binary, Path/binary >>.


basic_users_handler_test(Config) ->
    Client = ?config(client, Config),
    {ok, Request1} = cowboy_client:request(
        <<"GET">>,
        build_url(<<"/basic/users">>, Config),
        [{<<"accept">>, <<"application/json">>}],
        Client),
    {ok, 200, _, _} = cowboy_client:response(Request1),

    {ok, Request2} = cowboy_client:request(
        <<"GET">>,
        build_url(<<"/basic/users/1">>, Config),
        [{<<"accept">>, <<"application/json">>}],
        Client),
    {ok, 404, _, _} = cowboy_client:response(Request2).


basic_user_handler_with_error_handler_test(Config) ->
    Client = ?config(client, Config),
    {ok, Request1} = cowboy_client:request(
        <<"GET">>,
        build_url(<<"/basic_with_error_handler/users">>, Config),
        [{<<"accept">>, <<"application/json">>}],
        Client),
    {ok, 200, _, _} = cowboy_client:response(Request1),

    {ok, Request2} = cowboy_client:request(
        <<"GET">>,
        build_url(<<"/basic_with_error_handler/users/1">>, Config),
        [{<<"accept">>, <<"application/json">>}],
        Client),
    {ok, 404, _, _} = cowboy_client:response(Request2),

    {ok, Request3} = cowboy_client:request(
        <<"GET">>,
        build_url(<<"/basic_with_error_handler/users/error_id">>, Config),
        [{<<"accept">>, <<"application/json">>}],
        Client),
    {ok, 400, _, _} = cowboy_client:response(Request3),

    {ok, Request4} = cowboy_client:request(
        <<"GET">>,
        build_url(<<"/basic_with_error_handler/users/custom_error_id">>, Config),
        [{<<"accept">>, <<"application/json">>}],
        Client),
    {ok, 400, _, _} = cowboy_client:response(Request4),

    {ok, Request5} = cowboy_client:request(
        <<"GET">>,
        build_url(<<"/basic_with_error_handler/users/throw_id">>, Config),
        [{<<"accept">>, <<"application/json">>}],
        Client),
    {ok, 400, _, _} = cowboy_client:response(Request5).


users_handler_with_transitions_test(Config) ->
    Client = ?config(client, Config),
    {ok, Request1} = cowboy_client:request(
        <<"GET">>,
        build_url(<<"/transitions/users">>, Config),
        [{<<"accept">>, <<"application/json">>}],
        Client),
    {ok, 200, _, _} = cowboy_client:response(Request1).


encode_decode_handler_test(Config) ->
    Client = ?config(client, Config),
    {ok, Request1} = cowboy_client:request(
        <<"GET">>,
        build_url(<<"/encode_decode">>, Config),
        [],
        <<"{}">>,
        Client),
    {ok, 200, _, _} = cowboy_client:response(Request1),

    {ok, Request2} = cowboy_client:request(
        <<"GET">>,
        build_url(<<"/encode_decode">>, Config),
        [{<<"accept">>, <<"application/x-msgpack">>}],
        <<"{}">>,
        Client),
    {ok, 406, _, _} = cowboy_client:response(Request2),

    {ok, Request3} = cowboy_client:request(
        <<"GET">>,
        build_url(<<"/encode_decode">>, Config),
        [{<<"content-type">>, <<"application/x-msgpack">>}],
        <<"">>,
        Client),
    {ok, 415, _, _} = cowboy_client:response(Request3),
    ok.