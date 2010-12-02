%% @author Richard Jones <rj@metabrew.com>
%%
%% Process for handling send/recv on an established websocket
%% providing an 'active' api, ala gen_tcp in active mode.
%%
%% @see http://www.whatwg.org/specs/web-socket-protocol/
%% As of August 2010
%%
%% However, at time of writing (Oct 8, 2010) Chrome 6 and Firefox 4 implement
%% an older version of the websocket spec, where messages are framed 0x00...0xFF
%% so the newer protocol with length headers has not been tested with a browser.
%%
%% Guarantees that 'closed' will be sent to the client pid once the socket dies,
%% Messages are:
%%  closed, {error, Reason}, {frame, Data}

-module(mochiweb_websocket_delegate).
-behaviour(gen_server).

-record(state, {legacy,     %% version of websocket protocol
                socket,     %% mochiweb_socket 
                dest,       %% pid of client api process, destination for frames
                buffer,     %% rcv buffer
                partial,    %% current partially received frame
                ft,         %% frame type. 
                flen        %% current frame length, if known 
               }).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([start_link/1, go/2, send/2, close/1]).

%%

start_link(Destination) ->
    gen_server:start_link(?MODULE, [Destination], []).

go(Pid, Socket) ->
    ok = mochiweb_socket:controlling_process(Socket, Pid),
    gen_server:cast(Pid, {go, Socket}).

send(Pid, Msg) ->
    gen_server:call(Pid, {send, Msg}).

close(Pid) ->
    gen_server:call(Pid, close).

%%


init([Dest]) ->
    process_flag(trap_exit, true),
    {ok, #state{legacy  = true,
                dest    = Dest, 
                ft      = undefined,
                buffer  = <<>>,
                partial = <<>>
               }}.

handle_call(close, _From, State) ->
    mochiweb_socket:close(State#state.socket),
    {reply, ok, State};    
handle_call({send, Msg}, _From, State = #state{legacy=false, socket=Socket}) ->
    %% header is 0xFF then 64bit big-endian int of the msg length
    Len = iolist_size(Msg),
    R = mochiweb_socket:send(Socket, [255, <<Len:64/unsigned-integer>>, Msg]), 
    {reply, R, State};
handle_call({send, Msg}, _From, State = #state{legacy=true, socket=Socket}) ->
    %% legacy spec, msgs are framed with 0x00..0xFF
    R = mochiweb_socket:send(Socket, [0, Msg, 255]),
    {reply, R, State}.

handle_cast({go, Socket}, State) ->
    mochiweb_socket:setopts(Socket, [{active, true}]),    
    {noreply, State#state{socket=Socket}}.

handle_info({'EXIT', _, _}, State) ->
    State#state.dest ! closed,
    {stop, normal, State};
handle_info({Closed, _Sock}, State) when Closed =:= tcp_closed; 
                                         Closed =:= ssl_closed ->
    State#state.dest ! closed,
    {stop, normal, State};
handle_info({Error, _Sock, Reason}, State) when Error =:= tcp_error;
                                                Error =:= ssl_error ->
    State#state.dest ! {error, Reason},
    State#state.dest ! closed,
    {stop, normal, State};
handle_info({SockType, S, Data}, State = #state{socket=S, buffer=Buffer}) when SockType =:= tcp; 
                                                                               SockType =:= ssl ->
    NewState = process_data(State#state{buffer= <<Buffer/binary,Data/binary>>}),
    {noreply, NewState};
handle_info({ssl, _Sock, Data}, State = #state{buffer=Buffer}) ->
    NewState = process_data(State#state{buffer= <<Buffer/binary,Data/binary>>}),
    {noreply, NewState}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% Internal functions

process_data(State = #state{buffer= <<>>}) -> 
    State;

process_data(State = #state{buffer= <<FrameType:8,Buffer/binary>>, ft=undefined}) ->
    process_data(State#state{buffer=Buffer, ft=FrameType, partial= <<>>});

%% "Legacy" frames, 0x00...0xFF
%% or modern closing handshake 0x00{8}
process_data(State = #state{buffer= <<0,0,0,0,0,0,0,0, Buffer/binary>>, ft=0}) ->
    State#state.dest ! closing_handshake,
    process_data(State#state{buffer=Buffer, ft=undefined});

process_data(State = #state{buffer= <<255, Rest/binary>>, ft=0}) ->
    %% message received in full
    State#state.dest ! {frame, State#state.partial},
    process_data(State#state{partial= <<>>, ft=undefined, buffer=Rest});

process_data(State = #state{buffer= <<Byte:8, Rest/binary>>, ft=0, partial=Partial}) ->
    NewPartial = case Partial of 
                     <<>> -> <<Byte>>; 
                     _    -> <<Partial/binary, <<Byte>>/binary>> 
                 end,
   process_data(State#state{buffer=Rest, partial=NewPartial});

%% "Modern" frames, starting with 0xFF, followed by 64 bit length
process_data(State = #state{buffer= <<Len:64/unsigned-integer,Buffer/binary>>, ft=255, flen=undefined}) ->
    BitsLen = Len*8,
    case Buffer of
        <<Frame:BitsLen/binary, Rest/binary>> ->            
            State#state.dest ! {frame, Frame},
            process_data(State#state{ft=undefined, flen=undefined, buffer=Rest});

        _ ->
            State#state{flen=Len, buffer=Buffer}
    end;

process_data(State = #state{buffer=Buffer, ft=255, flen=Len}) when is_integer(Len) ->
    BitsLen = Len*8,
    case Buffer of
        <<Frame:BitsLen/binary, Rest/binary>> ->            
            State#state.dest ! {frame, Frame},
            process_data(State#state{ft=undefined, flen=undefined, buffer=Rest});

        _ ->
            State#state{flen=Len, buffer=Buffer}
    end.

%%
%% Tests
%%
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).

websocket_frame_parser_test() ->
    %% simulate arrival of frames split over multiple messages
    %% Check it yields 3 frame messages, and the fragment is left in the buffer
    Packets = [<<0,"what on earth is a quaid?",255>>,
               <<0,"the quick ">>,
               <<"brown fox jumps ">>,
               <<"over the lazy dog",255>>,
               <<0,"and what's so good about smints?",255>>,
               <<0,"fragment">>],
    FakeState = #state{ legacy=true, 
                        dest=self(),  %% send to ourselves for testing
                        ft=undefined,
                        buffer= <<>>,
                        partial= <<>> },
    FinalState = lists:foldl(fun(Packet, State=#state{buffer=Buffer}) ->
                                process_data(State#state{buffer= <<Buffer/binary,Packet/binary>>})
                            end, FakeState, Packets),
    %% check we were sent 3 frame messages
    {frame, <<"what on earth is a quaid?">>} = receive_once(),
    {frame, <<"the quick brown fox jumps over the lazy dog">>} = receive_once(),
    {frame, <<"and what's so good about smints?">>} = receive_once(),
    undefined = receive_once(),
    %% and that the fragment is left over
    <<"fragment">> = FinalState#state.partial,
    <<>>           = FinalState#state.buffer,
    ok.

receive_once() ->
    receive 
        X -> X
    after 0 ->
        undefined
    end.

-endif.
