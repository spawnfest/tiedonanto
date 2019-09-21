%%%-------------------------------------------------------------------
%%% @author Mathieu Kerjouan <contact@steepath.eu>
%%% @copyright 2019 Mathieu Kerjouan
%%%
%%% @doc 
%%% @end
%%%-------------------------------------------------------------------
-module(tiedonanto_SUITE).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").

suite() ->
    [].

init_per_suite(Config) ->
    Config.

init_per_testcase(_Case, Config) ->
    Config.

end_per_suite(Config) ->
    Config.

end_per_testcase(_Case, Config) ->
    Config.

all() ->
    [ create_connectors
    , create_contents
    , create_rules
    ].

%%--------------------------------------------------------------------
%% @doc a connector control an end-point.
%% @end
%%--------------------------------------------------------------------
create_connectors(Config) ->
    ok.

%%--------------------------------------------------------------------
%% @doc create 
%% @end
%%--------------------------------------------------------------------
create_rules(Config) ->
    ok.

%%--------------------------------------------------------------------
%% @doc create a new content based on test connector
%% @end
%%--------------------------------------------------------------------
create_contents(Config) ->
    ok.


