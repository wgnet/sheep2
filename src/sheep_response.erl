-module(sheep_response).

-export([new_204/0, new_404/0, new_405/0, new_406/0, new_500/0, new_501/0, new/2]).

-include("sheep.hrl").

-spec new_204() -> sheep_response().
new_204() -> new(204, <<>>).


-spec new_404() -> sheep_response().
new_404() -> new(404, <<"Not found">>).


-spec new_405() -> sheep_response().
new_405() -> new(405, <<"Method not allowed">>).


-spec new_406() -> sheep_response().
new_406() -> new(406, <<"Not acceptable">>).


-spec new_500() -> sheep_response().
new_500() -> new(500, <<"Internal server error">>).


-spec new_501() -> sheep_response().
new_501() -> new(501, <<"Not implemented">>).


-spec new(http_code(), map() | binary()) -> sheep_response().
new(StatusCode, Message) ->
    sheep_http:response(#{status_code => StatusCode, body => Message}).
