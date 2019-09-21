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

-spec start(Type :: term(), Args :: list()) -> ok.
start(_StartType, _StartArgs) ->
    pg2:start(),
    tiedonanto_sup:start_link().

-spec stop(State :: term()) -> ok.
stop(_State) ->
    ok.

