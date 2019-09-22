%%%-------------------------------------------------------------------
%%% @author Mathieu Kerjouan <contact@steepath.eu>
%%% @copyright 2019 Mathieu Kerjouan
%%%
%%% @doc tiedonanto_connector module abstracts the management for each
%%%      connectors. A connectors is a module directly connected on a 
%%%      remote or local end-point based on a protocol. Actually, 
%%%      only tcp, ssl, http, https and udp protocols are supported.
%%% @end
%%%-------------------------------------------------------------------
-module(tiedonanto_connector).
-export([start_link/0, start_link/1, start_link/2]).
-export([callback_mode/0, init/1, terminate/3]).
-export([main/3]).
-export([start_child/1, start_child/2]).
-behavior(gen_statem).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> {ok, pid()}.
start_link() ->
    start_link([]).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec start_link(Args :: list()) -> {ok, pid()}.
start_link(Args) ->
    start_link(Args, []).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec start_link(Args :: list(), Opts :: list()) -> {ok, pid()}.
start_link(Args,Opts) ->
    gen_statem:start_link({local, ?MODULE}, ?MODULE, Args, Opts).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec callback_mode() -> list().
callback_mode() ->
    [ state_functions
    , state_enter
    ].

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec connectors() -> list().
connectors() ->
    [ tiedonanto_connector_tcp
    , tiedonanto_connector_udp
    , tiedonanto_connector_ssl
    , tiedonanto_connector_http
    ].

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec init_groups() -> list().
init_groups() ->
    [ pg2:create(Connector) || Connector <- connectors() ].

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec init(Args :: list()) -> {ok, atom(), term()}.
init(_Args) ->
    init_groups(),
    {ok, main, []}.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason :: term(), State :: term(), Data :: term()) -> ok.
terminate(_Reason, _State, _Data) ->
    ok.

%%--------------------------------------------------------------------
%% @doc main/3 is the main state to manage all connectors.
%% @end
%%--------------------------------------------------------------------
-spec main(enter, term(), term()) 
          -> {next_state, main, term()};
          (EventType :: term(), Event :: term(), Data :: term()) 
          -> {keep_state, Data :: term()}.
main(enter, _OldState, Data) ->
    {next_state, main, Data};
main(cast, {start, Type, Args}, Data) ->
    handle_start_child(Type, Args, Data);
main(EventType, Event, Data) ->
    logger:debug("got event type ~p: ~p", [EventType, Event]),
    {keep_state, Data}.

%%--------------------------------------------------------------------
%% @doc handle_start_child/3 is a private function, used to make a 
%%      strong separation between the main state (here main/3) and
%%      the data received. If the project become larger, this function
%%      can be stored in another module.
%% @end
%%--------------------------------------------------------------------
handle_start_child(tcp, Args, Data) ->
    tiedonanto_connector_tcp_sup:start_child(Args),    
    {keep_state, Data};
handle_start_child(udp, Args, Data) ->
    tiedonanto_connector_udp_sup:start_child(Args),
    {keep_state, Data};
handle_start_child(http, Args, Data) ->
    tiedonanto_connector_udp_sup:start_child(Args),
    {keep_state, Data};
handle_start_child(ssl, Args, Data) ->
    tiedonanto_connector_udp_sup:start_child(Args),
    {keep_state, Data};
handle_start_child(Else, Args, Data) ->
    logger:error("unsupported type ~p with argument ~p", [Else, Args]),
    {keep_state, Data}.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec start_child(Type :: atom()) -> ok.
start_child(Type) ->
    start_child(Type, []).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec start_child(Type :: atom(), Args :: term()) -> ok.
start_child(Type, Args) ->
    gen_statem:cast(?MODULE, {start, Type, Args}).
