%% @author Richard Jones <rj@metabrew.com>
%% Websocket Request wrapper. this is passed to the ws_loop in client code.
%% It talks to mochiweb_websocket_delegate, but hides the pid from the client
%% and has cache of useful properties.
%% Parts of API copied from mochiweb_request.erl
%%
-module(mochiweb_wsrequest, [Pid, Path, Headers, Peername, SocketType]).

-export([send/1, close/0, get/1, get_header_value/1, get_cookie_value/1]).

-define(SAVE_COOKIE, mochiweb_request_cookie).

get(path)       -> Path;
get(headers)    -> Headers;
get(peername)   -> Peername;
get(type)       -> SocketType;  %% plain or ssl
get(peer) ->
    case Peername of 
        {ok, {Addr={10, _, _, _}, _Port}} ->
            case get_header_value("x-forwarded-for") of
                undefined ->
                    inet_parse:ntoa(Addr);
                Hosts ->
                    string:strip(lists:last(string:tokens(Hosts, ",")))
            end;
        {ok, {{127, 0, 0, 1}, _Port}} ->
            case get_header_value("x-forwarded-for") of
                undefined ->
                    "127.0.0.1";
                Hosts ->
                    string:strip(lists:last(string:tokens(Hosts, ",")))
            end;
        {ok, {Addr, _Port}} ->
            inet_parse:ntoa(Addr);
        {error, enotconn} ->
            ""
    end.


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
