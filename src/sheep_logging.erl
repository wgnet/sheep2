-module(sheep_logging).

-export([
    request/2
]).

-include("sheep.hrl").

-spec request(cowboy_req:req(), http_code()) -> atom().
request(Req, StatusCode) ->
    % TODO: Provide ability to specify format for logging in handler module
    error_logger:info_msg(
        <<"[http] ~s ~s - \"~s ~s ~s\" ~w ~s">>, [
            % $remote_addr
            inet:ntoa(element(1, element(1, cowboy_req:peer(Req)))),
            % $host
            element(1, cowboy_req:header(<<"host">>, Req, <<"-">>)),
            % $request
            element(1, cowboy_req:method(Req)),
            element(1, cowboy_req:path(Req)),
            element(1, cowboy_req:version(Req)),
            % $status
            StatusCode,
            % $http_user_agent
            element(1, cowboy_req:header(<<"user-agent">>, Req, <<"-">>))
        ]).
