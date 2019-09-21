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

start(_StartType, _StartArgs) ->
    pg2:start(),
    tiedonanto_sup:start_link().

stop(_State) ->
    ok.

