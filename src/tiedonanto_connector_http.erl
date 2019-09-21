%%%-------------------------------------------------------------------
%%% @author Mathieu Kerjouan <contact@steepath.eu>
%%% @copyright 2019 Mathieu Kerjouan
%%%
%%% @doc 
%%% @end
%%%-------------------------------------------------------------------
-module(tiedonanto_connector_http).
-export([start_link/0, start_link/1, start_link/2]).
-export([init/1, callback_mode/0, terminate/3]).
-export([wait/3, active/3]).
-behavior(gen_statem).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> {ok, pid()}.
start_link() -> start_link([]).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec start_link(Args :: list()) -> {ok, pid()}.
start_link(Args) -> start_link(Args, []).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec start_link(Args :: list(), Opts :: list()) -> {ok, pid()}.
start_link(Args,Opts) ->
    gen_statem:start_link(?MODULE, Args, Opts).

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
-spec init(Args :: list()) -> {ok, atom(), term()}.
init(_Args) ->
    ok = pg2:join(tiedonanto_connector_http, self()),
    {ok, wait, []}.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason :: term(), State :: atom(), Data :: term()) 
               -> ok.
terminate(_Reason, _State, _Data) ->
    pg2:leave(tiedonanto_connector_http, self()),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
wait(enter, _OldState, Data) ->
    {next_state, wait, Data};
wait(_EventType, _Event, Data) ->
    {keep_state, Data}.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
active(enter, _OldState, Data) ->
    {next_state, active, Data};
active(_EventType, _Event, Data) ->
    {keep_state, Data}.
    


