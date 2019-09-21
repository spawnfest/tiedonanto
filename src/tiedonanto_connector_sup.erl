%%%-------------------------------------------------------------------
%%% @doc tiedonanto_sup is the main application supervisor.
%%% @end
%%%-------------------------------------------------------------------
-module(tiedonanto_connector_sup).
-behaviour(supervisor).
-export([start_link/0, start_link/1]).
-export([init/1]).
-export([connector/0, connector/1]).
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
    #{ strategy => simple_one_for_all
     , intensity => 0
     , period => 1 
     }.

%%--------------------------------------------------------------------
%% @doc connector/0
%% @end
%%--------------------------------------------------------------------
-spec connector() -> map().
connector() ->
    connector([]).

%%--------------------------------------------------------------------
%% @doc connector/1
%% @end
%%--------------------------------------------------------------------
-spec connector(Args :: list()) -> map().
connector(Args) ->
    #{ id => tiedonanto_connector
     , start => {tiedonanto_connector, start_link, Args}
     , type => worker
     }.    

%%--------------------------------------------------------------------
%% @doc child_specs/0
%% @end
%%--------------------------------------------------------------------
-spec child_specs() -> [map(), ...].
child_specs() ->
    [].

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
