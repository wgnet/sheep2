-module(encode_decode_handler).
-behaviour(sheep_http).

-export([init/3, read/2, sheep_init/2 ]).

-include("sheep.hrl").

-spec init(atom(), cowboy_req:req(), term()) -> tuple().
init(_Transport, Req, _Opts) ->
    {upgrade, protocol, sheep_http, Req, []}.


-spec sheep_init(#sheep_request{}, term()) -> {#sheep_response{}, term()}.
sheep_init(_Request, _Opts) ->
    Options =
        #sheep_options{
            decode_spec =
            #{
                <<"application/json">> =>
                fun(Data) ->
                    M = jiffy:decode(Data, [return_maps]),
                    M#{custom_encoder => ok}
                end
            },
            encode_spec =
            #{
                <<"application/json">> =>
                fun(Data) ->
                    jiffy:encode(Data#{custom_decoder => ok})
                end
            }
        },
    State = [],
    {Options, State}.


-spec read(#sheep_request{}, term()) -> #sheep_response{}.
read(#sheep_request{bindings = #{<<"kind">> := <<"empty">>}} = _Request, _State) ->
    #sheep_response{status_code = 204};

read(#sheep_request{bindings = #{<<"kind">> := <<"empty_404">>}} = _Request, _State) ->
    #sheep_response{status_code = 404};

read(#sheep_request{bindings = #{<<"kind">> := <<"undefined">>}} = _Request, _State) ->
    #sheep_response{status_code = 204};

read(#sheep_request{bindings = #{<<"kind">> := <<"invalid_payload">>}} = _Request, _State) ->
    Body = [answer, 42],
    #sheep_response{status_code = 200, body = Body};

read(#sheep_request{body = Body}, _State)->
    Body2 = if
                is_map(Body) -> Body#{readed => true};
                true -> #{readed => true}
            end,
    #sheep_response{status_code = 200, body = Body2}.
