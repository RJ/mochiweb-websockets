%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Web server for websocket.

-module(websocket_web).
-author('author <author@example.com>').

-export([start/1, stop/0, loop/2, wsloop/1]).

%% External API

start(Options) ->
    {DocRoot, Options1} = get_option(docroot, Options),
    Loop = fun (Req) ->
                   ?MODULE:loop(Req, DocRoot)
           end,
    WebSocketLoop = fun (Req) ->
                   ?MODULE:wsloop(Req)
           end,
    mochiweb_http:start([{name, ?MODULE}, {loop, Loop}, {wsloop, WebSocketLoop} | Options1]).

stop() ->
    mochiweb_http:stop(?MODULE).

loop(Req, DocRoot) ->
    "/" ++ Path = Req:get(path),
    case Req:get(method) of
        Method when Method =:= 'GET'; Method =:= 'HEAD' ->
            case Path of
                _ ->
                    Req:serve_file(Path, DocRoot)
            end;
        'POST' ->
            case Path of
                _ ->
                    Req:not_found()
            end;
        _ ->
            Req:respond({501, [], []})
    end.

wsloop(WebSocket) ->
    %% Get the data sent from the client
    Data = WebSocket:get_data(),
    %% Our example...
    case Data of
        %% On initial connect we get this message
        "client-connected" ->
            WebSocket:send("You are connected!"),
            wsloop(WebSocket);

        %% Other messages go here
        Other ->
            case Other of
                "exit" ->
                    Msg = "Seeya",
                    WebSocket:send(Msg);
                _ ->
                    Msg = "You Said: " ++ Other,
                    WebSocket:send(Msg),
                    wsloop(WebSocket)
            end
    end.



%% Internal API

get_option(Option, Options) ->
    {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.
