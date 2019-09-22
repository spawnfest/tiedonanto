%%%-------------------------------------------------------------------
%%% @author Mathieu Kerjouan <contact@steepath.eu>
%%% @copyright 2019 Mathieu Kerjouan
%%%
%%% @doc tiedonanto application ensure every required component is
%%%      correctly started. This application also starts the main
%%%      supervisor from tiedonanto_sup.
%%% @end
%%%-------------------------------------------------------------------
-module(tiedonanto_app).
-behaviour(application).
-export([start/2, stop/1]).
-export([default_env/0]).

%%--------------------------------------------------------------------
%% @doc default_env/0 returns the default configuration for tiedonanto
%%      application.
%% @end
%%--------------------------------------------------------------------
-spec default_env() -> list().
default_env() ->
    [ {tcp_pool, 100}
    , {http_pool, 100}
    , {ssl_pool, 100}
    , {udp_pool, 100}
    ].

%%--------------------------------------------------------------------
%% @doc default_env/0 returns the default configuration for tiedonanto
%%      application.
%% @end
%%--------------------------------------------------------------------
-spec set_default_env() -> list().
set_default_env() -> 
    [ application:set_env(tiedonanto, Par, Val) 
      || {Par, Val} <- default_env() ].

%%--------------------------------------------------------------------
%% @doc start/2 start tiedonanto application and initialize 
%%      environment.
%% @end
%%--------------------------------------------------------------------
-spec start(Type :: term(), Args :: list()) -> ok.
start(_StartType, _StartArgs) ->
    set_default_env(),
    pg2:start(),
    tiedonanto_sup:start_link().

%%--------------------------------------------------------------------
%% @doc stop the application.
%% @end
%%--------------------------------------------------------------------
-spec stop(State :: term()) -> ok.
stop(_State) ->
    ok.

