-module(users_handler_with_transitions).

-export([
    init/3,
    sheep_init/2,
    authorization/2,
    validation/2,
    paging/2,
    request_mutation/2,
    read/2
]).

-include("sheep.hrl").

-record(state, {counter = 0}).


init(_Transport, Req, _Opts) ->
    {upgrade, protocol, sheep_http, Req, []}.


-spec sheep_init(sheep_request(), any()) -> {map(), any()}.
sheep_init(_Request, _Opts) ->
    Options =
        #{
            methods_spec =>
            #{
                <<"GET">> => [authorization, paging, request_mutation, read],
                <<"POST">> => [authorization, validation, create]
            }
        },
    State = #state{},
    {Options, State}.


authorization(Request, State) ->
    _Token = sheep_http:get_header(<<"token">>, Request),
    {noreply, State#state{counter = State#state.counter + 1}}.


validation(_Request, State) ->
    {noreply, State#state{counter = State#state.counter + 1}}.


paging(_Request, State) ->
    {noreply, State#state{counter = State#state.counter + 1}}.


request_mutation(Request, State) ->
    {noreply, Request#{key => value}, State}.


-spec read(sheep_request(), any()) -> {ok, sheep_response()}.
read(#{key := value} = _Request, #state{counter = Counter})->
    Data = [
        #{
            <<"id">> => <<"1">>,
            <<"name">> => <<"Username 1">>,
            <<"counter">> => Counter
        },
        #{
            <<"id">> => <<"2">>,
            <<"name">> => <<"Username 2">>
        }
    ],
    {ok, sheep_http:response(#{status_code => 200, body => Data})}.
