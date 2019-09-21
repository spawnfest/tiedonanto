%%%-------------------------------------------------------------------
%%% @author Mathieu Kerjouan <contact@steepath.eu>
%%% @copyright 2019 Mathieu Kerjouan
%%%
%%% @doc 
%%% @end
%%%-------------------------------------------------------------------
-module(tiedonanto_cli).
-compile(export_all).

main(Args) ->
    {ok, Args}.

init(_Args) ->
    application:ensure_all_started(tiedonanto).
