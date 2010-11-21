-module(websockets_demo).
-author('author <rj@metabrew.com>').

-export([start/0, start/1, stop/0, loop/2, wsloop_active/1]).

start() -> start([{port, 8080}, {docroot, "."}]).

start(Options) ->
    {DocRoot, Options1} = get_option(docroot, Options),
    Loop = fun (Req) -> ?MODULE:loop(Req, DocRoot) end,
    % How we validate origin for cross-domain checks:
    OriginValidator = fun(Origin) ->
                           io:format("Origin '~s' -> OK~n", [Origin]),
                           true
                      end,
    % websocket options
    WsOpts  = [ {active, true},
                {origin_validator, OriginValidator},
                {loop,   {?MODULE, wsloop_active}} ],
    %
    Ssl = [ {ssl, true}, {ssl_opts, [ {certfile, "../https/server_cert.pem"},
                                      {keyfile, "../https/server_key.pem"}]} ],
    %
    mochiweb_http:start([{name, ?MODULE}, 
                         {loop, Loop},
                         {websocket_opts, WsOpts} | Options1] ++ Ssl).

stop() ->
    mochiweb_http:stop(?MODULE).

wsloop_active(WSReq) ->
    % assuming you set a "session" cookie as part of your http login stuff
    io:format("session cookie: ~p~n", [WSReq:get_cookie_value("session")]),
    WSReq:send("WELCOME MSG FROM THE SERVER!"),
    % send some misc info to demonstrate the WSReq API
    Info = [ "Here's what the server knows about the connection:",
             "\nget(peername) = " , io_lib:format("~p",[WSReq:get(peername)]),
             "\nget(path) = " ,     io_lib:format("~p",[WSReq:get(path)]),
             "\nget(type) = " ,     io_lib:format("~p",[WSReq:get(type)]),
             "\nget(headers) = " ,  io_lib:format("~p",[WSReq:get(headers)]) ],
    WSReq:send(Info),
    wsloop_active0(WSReq).

wsloop_active0(WSReq) ->
    receive
        closed ->
            io:format("client api got closed~n",[]),
            ok;
        {error, Reason} ->
            io:format("client api got error ~p~n", [Reason]),
            ok;
        {frame, Frame} ->
            Msg = ["Dear client, thanks for sending us this msg: ", Frame],
            WSReq:send(Msg),
            wsloop_active0(WSReq)
    after 15000 ->
            WSReq:send("IDLE!"),
            wsloop_active0(WSReq)
    end.

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

%% Internal API

get_option(Option, Options) ->
    {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.
