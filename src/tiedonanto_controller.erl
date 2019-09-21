%%%-------------------------------------------------------------------
%%% @author Mathieu Kerjouan <contact@steepath.eu>
%%% @copyright 2019 Mathieu Kerjouan
%%%
%%% @doc tiedonanto_controller is the sub-application controller, used
%%%      to manage dynamic end-point and static one.
%%% @end
%%%-------------------------------------------------------------------
-module(tiedonanto_controller).
-export([start_link/0, start_link/1, start_link/2]).
-export([stop/0]).
-export([init/1, terminate/2]).
-export([handle_call/3, handle_cast/2]).
-behavior(gen_server).
-record(state, {db}).

%%--------------------------------------------------------------------
%% @doc start_link/0
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> {ok, pid()}.
start_link() ->
    start_link([]).

%%--------------------------------------------------------------------
%% @doc start_link/1
%% @end
%%--------------------------------------------------------------------
-spec start_link(Args :: list()) -> {ok, pid()}.
start_link(Args) ->
    start_link(Args, []).

%%--------------------------------------------------------------------
%% @doc start_link/2
%% @end
%%--------------------------------------------------------------------
-spec start_link(Args :: list(), Opts :: list()) -> {ok, pid()}.
start_link(Args, Opts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, Opts).

%%--------------------------------------------------------------------
%% @doc stop/0
%% @end
%%--------------------------------------------------------------------
-spec stop() -> ok.
stop() ->
    gen_server:stop(?MODULE).

%%--------------------------------------------------------------------
%% @doc init/1
%% @end
%%--------------------------------------------------------------------
-spec init(Args :: list()) -> {ok, #state{}}.
init(_Args) ->
    State = #state{ db = ets:new(?MODULE, [])},
    {ok, State}.

%%--------------------------------------------------------------------
%% @doc terminate/2
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason :: term(), State :: #state{}) -> ok.
terminate(_Reason, State) ->
    ok.

%%--------------------------------------------------------------------
%% @doc handle_call/3
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Data :: term(), From :: term(), State :: #state{}) -> {reply, term(), #state{}}.
handle_call(Data, From, State) ->
    {reply, Data, State}.

%%--------------------------------------------------------------------
%% @doc handle_cast/2
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(Data :: term(), State :: #state{}) -> {noreply, #state{}}.
handle_cast(Data, State) ->
    {noreply, State}.
