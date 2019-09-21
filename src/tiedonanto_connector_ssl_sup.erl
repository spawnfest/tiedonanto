%%%-------------------------------------------------------------------
%%% @author Mathieu Kerjouan <contact@steepath.eu>
%%% @copyright 2019 Mathieu Kerjouan
%%%
%%% @doc 
%%% @end
%%%-------------------------------------------------------------------
-module(tiedonanto_connector_ssl_sup).
-export([start_link/0, start_link/1]).
-export([init/1]).
-export([child_spec/0, child_spec/1]).
-behavior(supervisor).
-include_lib("eunit/include/eunit.hrl").

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
-spec start_link(Args :: term()) -> {ok, term()}.
start_link(Args) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Args).

start_link1_test() ->
    {ok, Pid} = start_link(),
    ?assertEqual(erlang:is_pid(Pid), true),
    ?assertEqual(erlang:whereis(?MODULE), Pid).

%%--------------------------------------------------------------------
%% @doc supervisor_flags/0
%% @end
%%--------------------------------------------------------------------
-spec supervisor_flags() -> map().
supervisor_flags() ->
    #{ strategy => simple_one_for_one
     , intensity => 0
     , period => 1 
     }.

%%--------------------------------------------------------------------
%% @doc child_spec/0
%% @end
%%--------------------------------------------------------------------
-spec child_spec() -> map().
child_spec() -> child_spec([]).

%%--------------------------------------------------------------------
%% @doc child_spec/1
%% @end
%%--------------------------------------------------------------------
-spec child_spec(Args :: term()) -> map().
child_spec(Args) ->
    #{ id => tiedonanto_connector_ssl
     , start => {tiedonanto_connector_ssl, start_link, Args}
     , type => worker
     }.

%%--------------------------------------------------------------------
%% @doc child_specs/0
%% @end
%%--------------------------------------------------------------------
-spec child_specs() -> [map(), ...].
child_specs() ->
    [child_spec()].

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

