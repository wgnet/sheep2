-module(sheep_response).

-export([new_204/0, new_404/0, new_405/0, new_406/0, new_500/0, new_501/0]).

-include("sheep.hrl").

-spec new_204() -> sheep_response().
new_204() -> #sheep_response{status_code = 204}.


-spec new_404() -> sheep_response().
new_404() -> #sheep_response{status_code = 404, body = <<"Not found">>}.


-spec new_405() -> sheep_response().
new_405() -> #sheep_response{status_code = 405, body = <<"Method not allowed">>}.


-spec new_406() -> sheep_response().
new_406() -> #sheep_response{status_code = 406, body = <<"Not acceptable">>}.


-spec new_500() -> sheep_response().
new_500() -> #sheep_response{status_code = 500, body = <<"Internal server error">>}.


-spec new_501() -> sheep_response().
new_501() -> #sheep_response{status_code = 501, body = <<"Not implemented">>}.
