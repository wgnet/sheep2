-module(sheep_try).

-export([run/0, query/4, log/1]).
-export([init/2, read/2]).

-include("sheep.hrl").


-spec run() -> {integer(), list(), binary()}.
run() ->
    application:set_env(sheep2, log_callback, fun ?MODULE:log/1),
    application:ensure_all_started(cowboy),
    hackney:start(),

    Routing = cowboy_router:compile([
        {"localhost", [
            {"/[:param]", sheep_try, []}
        ]}
    ]),

    {ok, _} = cowboy:start_clear(?MODULE, [{port, 0}],
        #{
            env => #{dispatch => Routing},
            max_keepalive => 50,
            timeout => 500
        }),

    H = [
        {<<"content-type">>, <<"application/json">>}
        %% {<<"accept">>, <<"application/json">>}
    ],
    query(get, "/hello", H, <<>>).


-spec query(atom(), string(), list(), binary()) -> {integer(), list(), binary()}.
query(Method, Path, Headers, Data) ->
    Port = integer_to_list(ranch:get_port(?MODULE)),
    FullURL = "http://localhost:" ++ Port ++ Path,
    {ok, Status, RHeaders, Ref} = hackney:request(Method, FullURL, Headers, Data),
    {ok, Body} = hackney:body(Ref),
    {Status, RHeaders, Body}.


-spec log({cowboy_req:req(), #sheep_request{}, #sheep_response{}}) -> ok.
log({Req, Request, Response}) ->
    {RAddr, _RPort} = cowboy_req:peer(Req),
    RemoteAddr = inet:ntoa(RAddr),
    Host = cowboy_req:header(<<"host">>, Req, <<"-">>),
    Method = cowboy_req:method(Req),
    Path = cowboy_req:path(Req),

    io:format("Log:~n"),
    io:format(" Cowboy req: ~s ~s - \"~s ~s\"~n", [RemoteAddr, Host, Method, Path]),
    io:format(" Sheep request: ~p~n", [Request]),
    io:format(" Sheep response: ~p~n", [Response]),
    ok.


%% Sheep handler

-spec init(cowboy_req:req(), term()) -> tuple().
init(Req, Opts) ->
    {sheep_http, Req, Opts}.


-spec read(#sheep_request{}, term()) -> #sheep_response{}.
read(_Request, _State)->
    Body = #{<<"answer">> => 42},
    #sheep_response{status_code = 200, body = Body}.
