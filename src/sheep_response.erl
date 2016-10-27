-module(sheep_response).

-export([new_204/0, new_404/0, new_405/0, new_406/0, new_500/0, new_501/0, new/2]).

new_204() ->
    new(204, <<>>).

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
