%%%-------------------------------------------------------------------
%%% @doc tiedonanto_sup is the main application supervisor.
%%% @end
%%%-------------------------------------------------------------------
-module(tiedonanto_sup).
-behaviour(supervisor).
-export([start_link/0, start_link/1]).
-export([init/1]).
-type args() :: list().

%%--------------------------------------------------------------------
%% @doc start_link/0
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> {ok, term()}.
start_link() ->
    start_link([]).

%%--------------------------------------------------------------------
%% @doc start_link/1
%% @end
%%--------------------------------------------------------------------
-spec start_link(Args :: args()) -> {ok, term()}.
start_link(Args) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Args).

%%--------------------------------------------------------------------
%% @doc supervisor_flags/0
%% @end
%%--------------------------------------------------------------------
-spec supervisor_flags() -> map().
supervisor_flags() ->
    #{ strategy => one_for_one
     , intensity => 0
     , period => 1 
     }.

%%--------------------------------------------------------------------
%% @doc controller_sup/0
%% @end
%%--------------------------------------------------------------------
-spec controller_sup() -> map().
controller_sup() ->
    #{ id => tiedonanto_controller_sup
     , start => {tiedonanto_controller_sup, start_link, []}
     , type => supervisor
     }.

%%--------------------------------------------------------------------
%% @doc controller_sup/0
%% @end
%%--------------------------------------------------------------------
-spec connector_sup() -> map().
connector_sup() ->
    #{ id => tiedonanto_connector_sup
     , start => {tiedonanto_connector_sup, start_link, []}
     , type => supervisor
     }.

%%--------------------------------------------------------------------
%% @doc child_specs/0
%% @end
%%--------------------------------------------------------------------
-spec child_specs() -> [map(), ...].
child_specs() ->
    [ controller_sup()
    , connector_sup()
    ].

%%--------------------------------------------------------------------
%% @doc supervisor_state/0
%% @end
%%--------------------------------------------------------------------
-spec supervisor_state() -> {map(), [map(), ...]}.
supervisor_state() ->
    { supervisor_flags()
    , child_specs() 
    }.

%%--------------------------------------------------------------------
%% @doc init/1
%% @end
%%--------------------------------------------------------------------
-spec init(list()) -> {ok, {map(), [map(), ...]}}.
init(_Args) ->
    {ok, supervisor_state()}.
