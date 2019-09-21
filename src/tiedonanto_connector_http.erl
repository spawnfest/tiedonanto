%%%-------------------------------------------------------------------
%%% @author Mathieu Kerjouan <contact@steepath.eu>
%%% @copyright 2019 Mathieu Kerjouan
%%%
%%% @doc 
%%% @end
%%%-------------------------------------------------------------------
-module(tiedonanto_connector_http).
-behavior(gen_statem).
-compile(export_all).

start_link() ->
    start_link([]).

start_link(Args) ->
    start_link(Args, []).

start_link(Args,Opts) ->
    gen_statem:start_link(?MODULE, Args, Opts).

callback_mode() ->
    [state_functions, state_enter].

init(_Args) ->
    {ok, []}.

terminate(_Reason, _State, _Data) ->
    ok.
