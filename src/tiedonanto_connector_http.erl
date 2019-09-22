%%%-------------------------------------------------------------------
%%% @author Mathieu Kerjouan <contact@steepath.eu>
%%% @copyright 2019 Mathieu Kerjouan
%%%
%%% @doc tiedonanto_connector_http module manage all dynamically
%%%      http connector. To make things easier to understand, it
%%%      spawn and control all http connector linked to 
%%%      tiedonanto_connector_http_sup supervisor.
%%% @end
%%%-------------------------------------------------------------------
-module(tiedonanto_connector_http).
-export([start_link/0, start_link/1, start_link/2]).
-export([init/1, callback_mode/0, terminate/3]).
-export([wait/3, active/3]).
-behavior(gen_statem).
-record(struct, { target = undefined
                , port = undefined 
                , path = undefined 
                , socket = undefined 
                , opts = [] 
                }).

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
    ok = pg2:join(?MODULE, self()),
    {ok, wait, #struct{}}.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason :: term(), State :: atom(), Data :: term()) 
               -> ok.
terminate(_Reason, _State, _Data) ->
    pg2:leave(?MODULE, self()),
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
    


