%% @author Richard Jones <rj@metabrew.com>
%% Websocket Request wrapper. this is passed to the ws_loop in client code.
%% It talks to mochiweb_websocket_delegate, but hides the pid from the client.
%% and has cache of useful properties.
-module(mochiweb_wsrequest, [Pid, Path, Headers, Peername, SocketType]).

-export([send/1, close/0, get/1, get_header_value/1, get_cookie_value/1]).

-define(SAVE_COOKIE, mochiweb_request_cookie).

get(path)       -> Path;
get(headers)    -> Headers;
get(peername)   -> Peername;
get(type)       -> SocketType.  % plain or ssl

send(Msg)       -> mochiweb_websocket_delegate:send(Pid, Msg).

close()         -> mochiweb_websocket_delegate:close(Pid).

%% @spec get_header_value(K) -> undefined | Value
%% @doc Get the value of a given request header.
get_header_value(K) ->
    mochiweb_headers:get_value(K, Headers).

%% @spec get_cookie_value(Key::string) -> string() | undefined
%% @doc Get the value of the given cookie.
get_cookie_value(Key) ->
    proplists:get_value(Key, parse_cookie()).

%% @spec parse_cookie() -> [{Key::string(), Value::string()}]
%% @doc Parse the cookie header.
parse_cookie() ->
    case erlang:get(?SAVE_COOKIE) of
        undefined ->
            Cookies = case get_header_value("cookie") of
                          undefined ->
                              [];
                          Value ->
                              mochiweb_cookies:parse_cookie(Value)
                      end,
            put(?SAVE_COOKIE, Cookies),
            Cookies;
        Cached ->
            Cached
    end.
