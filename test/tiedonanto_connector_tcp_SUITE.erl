%%%-------------------------------------------------------------------
%%% @author Mathieu Kerjouan <contact@steepath.eu>
%%% @copyright 2019 Mathieu Kerjouan
%%%
%%% @doc 
%%% @end
%%%-------------------------------------------------------------------
-module(tiedonanto_connector_tcp_SUITE).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").

suite() ->
    [].

init_per_suite(Config) ->
    {ok, Supervisor} = tiedonanto_connector_tcp_sup:start_link(),
    [{supervisor, Supervisor}|Config].

init_per_testcase(_Case, Config) ->
    Config.

end_per_suite(Config) ->
    Config.

end_per_testcase(_Case, Config) ->
    Config.

all() ->
    [ create_connector_tcp 
    ].

create_connector_tcp(Config) ->
    Supervisor = proplists:get_value(supervisor, Config),
    
    ok.
    
