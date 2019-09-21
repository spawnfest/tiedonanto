%%%-------------------------------------------------------------------
%%% @author Mathieu Kerjouan <contact@steepath.eu>
%%% @copyright 2019 Mathieu Kerjouan
%%%
%%% @doc 
%%% @end
%%%-------------------------------------------------------------------
-module(tiedonanto_controller_sup).
-export([start_link/0, start_link/1]).
-export([init/1]).
-export([supervisor_flags/0, child_specs/0]).
-behavior(supervisor).

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
-spec start_link(Args :: list()) -> {ok, term()}.
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
%% @doc controller_spec/0
%% @end
%%--------------------------------------------------------------------
-spec controller_spec() -> map().
controller_spec() ->
    #{ id => tiedonanto_controller
     , start => {tiedonanto_controller, start_link, []}
     , type => worker
     }.

%%--------------------------------------------------------------------
%% @doc child_specs/0
%% @end
%%--------------------------------------------------------------------
-spec child_specs() -> [map(), ...].
child_specs() ->
    [controller_spec()].

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
-spec init(Args :: list()) -> {ok, {map(), [map(), ...]}}.
init(_Args) ->
    {ok, supervisor_state()}.

