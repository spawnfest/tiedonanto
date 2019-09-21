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

start_link() ->
    start_link([]).

start_link(Args) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Args).

supervisor_flags() ->
    #{ strategy => one_for_all
     , intensity => 0
     , period => 1 }.

controller_spec() ->
    #{ id => tiedonanto_controller
     , start => {tiedonanto_controller, start_link, []}
     , type => worker
     }.

child_specs() ->
    [controller_spec()].

supervisor_state() ->
    { supervisor_flags()
    , child_specs() }.

init(_Args) ->
    {ok, supervisor_state()}.

