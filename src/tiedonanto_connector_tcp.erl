%%%-------------------------------------------------------------------
%%% @author Mathieu Kerjouan <contact@steepath.eu>
%%% @copyright 2019 Mathieu Kerjouan
%%%
%%% @doc 
%%% @end
%%%-------------------------------------------------------------------
-module(tiedonanto_connector_tcp).
-behavior(gen_statem).
-export([start_link/0, start_link/1, start_link/2]).
-export([callback_mode/0, init/1, terminate/3]).
-export([wait/3, active/3]).
-export([connect/3, connect/4]).
-export([send/2]).
-include_lib("eunit/include/eunit.hrl").
-record(struct, { target = undefined
                , port = undefined
                , opts = []
                , socket = undefined
                }).

%%--------------------------------------------------------------------
%% @doc start_link/0
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> {ok, pid()}.
start_link() -> start_link([]).

%%--------------------------------------------------------------------
%% @doc start_link/1
%% @end
%%--------------------------------------------------------------------
-spec start_link(Args :: list()) -> {ok, pid()}.
start_link(Args) -> start_link(Args, []).

%%--------------------------------------------------------------------
%% @doc start_link/2
%% @end
%%--------------------------------------------------------------------
-spec start_link(Args :: list(), Opts :: list()) -> {ok, pid()}.
start_link(Args, Opts) ->
    gen_statem:start_link(?MODULE, Args, Opts).

%%--------------------------------------------------------------------
%% @doc callback_mode/0
%% @end
%%--------------------------------------------------------------------
-spec callback_mode() -> list().
callback_mode() ->
    [ state_functions
    , state_enter
    ].

%%--------------------------------------------------------------------
%% @doc init/1
%% @end
%%--------------------------------------------------------------------
-spec init(Args :: list()) -> {ok, wait, term()}.
init(_Args) -> 
    logger:info("add ~p in ~p pg2 group", [self(), ?MODULE]),
    ok = pg2:join(?MODULE, self()),
    {ok, wait, []}.

%%--------------------------------------------------------------------
%% @doc terminate/3
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason :: term(), State :: atom(), Data :: term())
               -> ok.
terminate(_Reason, _State, _Data) ->
    pg2:leave(?MODULE, self()),
    ok.

%%--------------------------------------------------------------------
%% @doc wait/3 is a generic state function and can be used in other
%%      endpoint connector. Will try after to dynamically add it
%%      in tiedonanto_connector instead of each 
%%      tiedonanto_connector_*
%% @end
%%--------------------------------------------------------------------
wait(enter, _OldState, Data) ->
    {next_state, wait, Data};
wait({call, From}, {connect, {Target, Port, Opts}}, Data) ->
    case gen_tcp:connect(Target, Port, Opts) of
        {ok, Socket} -> 
            erlang:port_connect(Socket, self()),
            { next_state
            , active
            , #struct{ target = Target
                     , port = Port 
                     , opts = Opts 
                     , socket = Socket }
            , [{reply, From, ok}]
            };
        _Else -> 
            { keep_state
            , Data
            , [{reply, From, _Else}]
            }
    end;
wait({call, From}, _Event, Data) ->
    {keep_state, Data, [{reply, From, Data}]};
wait(EventType, Event, Data) ->
    {keep_state, Data}.

%%--------------------------------------------------------------------
%% @doc active/3 is a state function to control an active tcp
%%      connection. In wait state, the process wait for configuration
%%      (e.g. datastructure) comming from a controller. If the 
%%      data-structure is well configured, the process start a 
%%      tcp connection in active mode (directly connected to a port).
%% @end
%%--------------------------------------------------------------------
-spec active(atom() | tuple(), term(), term())
            -> tuple().
active(enter, _OldState, Data) ->
    {next_state, active, Data};
active(cast, {send, Message}, #struct{ socket = Socket } = Data) ->
    gen_tcp:send(Socket, Message),
    {keep_state, Data};
active({call, From}, {send, Message}, Data) ->
    {keep_state, Data, [{reply, From, ok}]};
active(info, {tcp, Port, Message}, #struct{ socket = Port } = Data) ->
    logger:info("receive ~p from ~p", [Port, Message]),
    {keep_state, Data};    
active(info, {tcp_closed, Port}, #struct{ socket = Port } = Data) ->
    gen_tcp:close(Port),
    {next_state, wait, Data#struct{ port = undefined }}.

%%--------------------------------------------------------------------
%% @doc connect/3 same as connect/4, setting Opts argument to [].
%% @end
%%--------------------------------------------------------------------
-spec connect( Pid :: pid()
             , Target :: atom() | tuple()
             , Port :: integer())
             -> {ok, term()} | {error, term()}.
connect(Pid, Target, Port) ->
    connect(Pid, Target, Port, []).

%%--------------------------------------------------------------------
%% @doc connect/4 connects to a remote end-point and switch the 
%%      current FSM to active state, waiting for message to send and
%%      receive.
%% @end
%%--------------------------------------------------------------------
-spec connect( Pid :: pid()
             , Target :: atom() | tuple()
             , Port :: integer()
             , Opts :: list())
             -> {ok, term()} | {error, term()}.
connect(Pid, Target, Port, Opts) ->
    gen_statem:call(Pid, {connect, {Target, Port, Opts}}).

%%--------------------------------------------------------------------
%% @doc send/2 gives the possibility to send a message to the defined
%%      process in first argument.
%% @end
%%--------------------------------------------------------------------
-spec send(Pid :: pid(), Message :: bitstring()) -> ok.
send(Pid, Message) ->
    gen_statem:cast(Pid, {send, Message}).
