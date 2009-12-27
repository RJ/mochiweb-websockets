%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc TEMPLATE.

-module(websocket).
-author('author <author@example.com>').
-export([start/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.
        
%% @spec start() -> ok
%% @doc Start the websocket server.
start() ->
    websocket_deps:ensure(),
    ensure_started(crypto),
    application:start(websocket).

%% @spec stop() -> ok
%% @doc Stop the websocket server.
stop() ->
    Res = application:stop(websocket),
    application:stop(crypto),
    Res.
