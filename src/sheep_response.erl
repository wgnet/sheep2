-module(sheep_response).

-compile([export_all]).

-include("sheep.hrl").

new_204() ->
    new(204, <<"">>).

new_404() ->
    new(404, <<"Not found">>).

new_405() ->
    new(405, <<"Method not allowed">>).

new_406() ->
    new(406, <<"Not acceptable">>).

new_500() ->
    new(500, <<"Internal server error">>).

new_501() ->
    new(501, <<"Not implemented">>).

new(StatusCode, Message) ->
    sheep_http:response(#{status_code => StatusCode, body => Message}).
