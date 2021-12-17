-module(sheep_try).

-export([run/0, query/4, log/1]).
-export([init/2, read/2]).

-include("sheep.hrl").


-spec run() -> {integer(), list(), binary()}.
run() ->
    application:set_env(sheep2, log_callback, fun ?MODULE:log/1),
    application:ensure_all_started(cowboy),
    application:ensure_all_started(gun),

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
    Port = ranch:get_port(?MODULE),
    {ok, Pid} = gun:open("localhost", Port),
    {ok, http} = gun:await_up(Pid),
    StreamRef = gun:request(Pid, method(Method), Path, Headers, Data),
    Opts = #{
        pid => Pid,
        stream_ref => StreamRef,
        acc => <<>>
    },
    #{status := Status, headers := RHeaders, acc := Body} = get_reponse(Opts),
    {Status, RHeaders, Body}.

-spec method(atom()) -> binary().
method(Method) ->
    cowboy_bstr:to_upper(erlang:list_to_binary(erlang:atom_to_list(Method))).

get_reponse(#{pid := Pid, stream_ref := StreamRef, acc := Acc} = Opts) ->
    case gun:await(Pid, StreamRef) of
        {response, fin, Status, Headers} ->
            Opts#{status => Status, headers => Headers};
        {response, nofin, Status, Headers} ->
            get_reponse(Opts#{status => Status, headers => Headers});
        {data, nofin, Data} ->
            get_reponse(Opts#{acc => <<Acc/binary, Data/binary>>});
        {data, fin, Data} ->
            Opts#{acc := <<Acc/binary, Data/binary>>};
        {error, timeout} = Response ->
            Response;
        {error, _Reason} = Response->
            Response
    end.

-spec log({cowboy_req:req(), sheep_request(), sheep_response()}) -> ok.
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


-spec read(sheep_request(), term()) -> sheep_response().
read(_Request, _State)->
    Body = #{<<"answer">> => 42},
    #sheep_response{status_code = 200, body = Body}.
