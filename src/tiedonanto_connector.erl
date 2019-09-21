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
-behavior(gen_statem).
-compile(export_all).

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
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec main(enter, term(), term()) 
          -> {next_state, main, term()};
          (EventType :: term(), Event :: term(), Data :: term()) 
          -> {keep_state, Data :: term()}.
main(enter, _OldState, Data) ->
    {next_state, main, Data};
main(EventType, Event, Data) ->
    {keep_state, Data}.
    


