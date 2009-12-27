%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Callbacks for the websocket application.

-module(websocket_app).
-author('author <author@example.com>').

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for websocket.
start(_Type, _StartArgs) ->
    websocket_deps:ensure(),
    websocket_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for websocket.
stop(_State) ->
    ok.
