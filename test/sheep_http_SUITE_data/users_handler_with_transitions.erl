-module(users_handler_with_transitions).
-behaviour(sheep_http).

-export([
    init/3,
    sheep_init/2,
    authorization/2,
    paging/2,
    validation/2,
    read/2,
    create/2
]).

-include("sheep.hrl").

-record(state, {
    steps = [],
    user_id
}).


init(_Transport, Req, _Opts) ->
    {upgrade, protocol, sheep_http, Req, []}.


-spec sheep_init(sheep_request(), any()) -> {map(), any()}.
sheep_init(_Request, _Opts) ->
    Options =
        #{
            methods_spec =>
            #{
                <<"GET">> => [authorization, paging, read],
                <<"POST">> => [authorization, validation, create]
            }
        },
    State = #state{},
    {Options, State}.


authorization(Request, #state{steps = Steps} = State) ->
    Token = sheep_http:get_header(<<"x-auth-token">>, Request),
    case Token of
        <<"cft6GLEhLANgstU8sZdL">> ->
            {noreply, State#state{steps = [<<"auth">> | Steps]}};
        _ ->
            {error, sheep_response:new(401, <<"Auth error">>)}
    end.


paging(_Request, #state{steps = Steps} = State) ->
    {noreply, State#state{steps = [<<"paging">> | Steps]}}.


validation(#{body := Body}, #state{steps = Steps} = State) ->
    case Body of
        #{<<"user_id">> := UserID} ->
            {noreply, State#state{steps = [<<"validation">> | Steps], user_id = UserID}};
        _ -> {error, sheep_response:new(400, #{<<"error">> => <<"User ID not provided">>})}
    end.


-spec read(sheep_request(), any()) -> {ok, sheep_response()}.
read(_Request, #state{steps = Steps}) ->
    Body = #{
        <<"reply_from">> => <<"read">>,
        <<"steps">> => Steps
    },
    {ok, sheep_http:response(#{status_code => 200, body => Body})}.


-spec create(sheep_request(), any()) -> {ok, sheep_response()}.
create(_Request, #state{steps = Steps, user_id = UserID}) ->
    Body = #{
        <<"reply_from">> => <<"create">>,
        <<"steps">> => Steps,
        <<"user_id">> => UserID
    },
    {ok, sheep_http:response(#{status_code => 200, body => Body})}.
