-module(encode_decode_handler).

-export([
    init/3
]).

% Handlers
-export([
    read/2,
    sheep_init/2
]).

-include("sheep.hrl").

init(_Transport, Req, _Opts) ->
    {upgrade, protocol, sheep_http, Req, []}.

-spec sheep_init(map(), any()) -> {list(), any()}.
sheep_init(_Request, _Opts) ->
    {[
        {decode_spec, [
            {<<"application/json">>,
                fun(Data) ->
                    {L} = jiffy:decode(Data),
                    M = maps:from_list(L),
                    M#{custom_encoder => ok}
                end}
        ]},
        {encode_spec, [
            {<<"application/json">>,
                fun(Data) ->
                    case Data of
                        _ when is_map(Data) ->
                            jiffy:encode(
                                {maps:to_list(Data#{custom_decoder => ok})});
                        _ ->
                            jiffy:encode(Data)
                    end
                end}
        ]}
    ],
    []}.

read(#{bindings := #{<<"kind">> := <<"empty">>}} = _Request, _State) ->
    {ok, sheep_http:response(#{})};

read(#{bindings := #{<<"kind">> := <<"empty_404">>}} = _Request, _State) ->
    {ok, sheep_http:response(#{status_code => 404})};

read(#{bindings := #{<<"kind">> := <<"undefined">>}} = _Request, _State) ->
    {ok, sheep_http:response(#{})};


% Get collection
read(#{body := Data}, _State)->
    {ok, sheep_http:response(#{status_code => 200, body => Data})}.
