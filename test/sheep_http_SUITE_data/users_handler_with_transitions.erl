-module(users_handler_with_transitions).

-export([
    init/3,
    sheep_init/2
]).

% Handlers
-export([
    authorization/2,
    validation/2,
    paged/2,
    request_mutation/2,
    read/2
]).

-include("sheep.hrl").

-record(state, {
    counter = 0
}).

init(_Transport, Req, _Opts) ->
    {upgrade, protocol, sheep_http, Req, []}.

-spec sheep_init(sheep_request(), any()) -> {list(), any()}.
sheep_init(_Request, _Opts) ->
    Options = [
        {
            methods_spec, [
                {<<"POST">>, [authorization, validation, create]},
                {<<"GET">>, [authorization, paged, request_mutation, read]},
                {<<"PUT">>, []},
                {<<"DELETE">>, []}
            ]
        },
        {
            access_log_format,
            "$remote_addr $host $request $status $http_user_agent"
        }
    ],
    State = #state{},
    {Options, State}.

authorization(Request, State) ->
    _Token = sheep_http:get_header(<<"token">>, Request),
    {noreply, State#state{counter = State#state.counter + 1}}.

validation(_Request, State) ->
    {noreply, State#state{counter = State#state.counter + 1}}.

paged(_Request, State) ->
    {noreply, State#state{counter = State#state.counter + 1}}.

request_mutation(Request, State) ->
    {noreply, Request#{key => value}, State}.

% Get collection
read(#{key := value} = _Request, #state{} = State)->
    Data = [
        {[
            {<<"id">>, <<"1">>},
            {<<"name">>, <<"Username 1">>}
        ]},
        {[
            {<<"id">>, <<"2">>},
            {<<"name">>, <<"Username 2">>}
        ]}
    ],
    {ok, sheep_http:response(#{status_code => 200, body => Data})}.
