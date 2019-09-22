%%%-------------------------------------------------------------------
%%% @author Mathieu Kerjouan <contact@steepath.eu>
%%% @copyright 2019 Mathieu Kerjouan
%%%
%%% @doc tiedonanto_connector_tcp_sup module manage all tcp client
%%%      connector by creating a pool of tcp client based on 
%%%      gen_tcp module and gen_statem FSM. This supervisor offers the
%%%      facilities to manage this pool.
%%% @end
%%%-------------------------------------------------------------------
-module(tiedonanto_connector_tcp_sup).
-export([start_link/0, start_link/1]).
-export([init/1]).
-export([child_spec/0, child_spec/1]).
-export([start_child/0, start_child/1]).
-behavior(supervisor).
-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------
%% @doc start_link/0, see start_link/1.
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> {ok, term()}.
start_link() ->
    start_link([]).

%%--------------------------------------------------------------------
%% @doc start_link/1 start a new tcp connector supervisor.
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
%% @doc supervisor_flags/0 return the specification for this 
%%      supervisor.
%% @end
%%--------------------------------------------------------------------
-spec supervisor_flags() -> map().
supervisor_flags() ->
    #{ strategy => simple_one_for_one
     , intensity => 0
     , period => 1 
     }.

%%--------------------------------------------------------------------
%% @doc child_spec/0 see child_spec/1.
%% @end
%%--------------------------------------------------------------------
-spec child_spec() -> map().
child_spec() -> child_spec([]).

%%--------------------------------------------------------------------
%% @doc child_spec/1 returns the default child specification used
%%      to spawn a new worker with parameter. This function will not
%%      be used in the code, because this supervisor use a 
%%      simple_one_for_one strategy and, if you want to spawn a new
%%      child, arguments will be added to the current Args parameter
%%      as list.
%% @end
%%--------------------------------------------------------------------
-spec child_spec(Args :: term()) -> map().
child_spec(Args) ->
    #{ id => tiedonanto_connector_tcp
     , start => {tiedonanto_connector_tcp, start_link, Args}
     , type => worker
     }.

%%--------------------------------------------------------------------
%% @doc child_specs/0 return the list of the child specifications, 
%%      this supervisor use a  simple_one_for_one method and no
%%      child will be started automatically.
%% @end
%%--------------------------------------------------------------------
-spec child_specs() -> [map(), ...].
child_specs() ->
    [ child_spec()
    ].

%%--------------------------------------------------------------------
%% @doc supervisor_state/0 return the data-structure required in
%%      supervisor init/1.
%% @end
%%--------------------------------------------------------------------
-spec supervisor_state() -> {map(), [map(), ...]}.
supervisor_state() ->
    { supervisor_flags()
    , child_specs() 
    }.

%%--------------------------------------------------------------------
%% @doc init/1 is a standard supervisor callback.
%% @end
%%--------------------------------------------------------------------
-spec init(list()) -> {ok, {map(), [map(), ...]}}.
init(_Args) ->
    {ok, supervisor_state()}.

%%--------------------------------------------------------------------
%% @doc start_child/0 start a new tcp connector with default argument,
%%      automatically connector to this supervisor.
%% @end
%%--------------------------------------------------------------------
-spec start_child() -> {ok, pid()}.
start_child() ->
    start_child([]).

%%--------------------------------------------------------------------
%% @doc start_child/1 start a new tcp connector with user defined
%%      parameters automatically connected to this supervisor.
%% @end
%%--------------------------------------------------------------------
-spec start_child(list()) -> {ok, pid()}.
start_child(Args) ->
    supervisor:start_child(?MODULE, Args).
