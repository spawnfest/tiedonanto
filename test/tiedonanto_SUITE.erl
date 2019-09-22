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
    application:ensure_all_started(tiedonanto),
    Config.

init_per_testcase(_Case, Config) ->
    Config.

end_per_suite(Config) ->
    application:stop(tiedonanto),
    Config.

end_per_testcase(_Case, Config) ->
    Config.

all() ->
    [ create_controllers
    , create_contents
    , create_rules
    ].

%%--------------------------------------------------------------------
%% @doc a connector control an end-point.
%% @end
%%--------------------------------------------------------------------
create_controllers(Config) ->
    {ok, test_controller, []} = tiedonanto:controller(test_controller, []),
    {ok, [{test_controller, []}]} = tiedonanto:controllers().

%%--------------------------------------------------------------------
%% @doc create 
%% @end
%%--------------------------------------------------------------------
create_rules(Config) ->
    TestRule = tiedonanto:rule(test_controller, test_rule, []),
    {ok, {test_controller, test_rule, []}} = TestRule.
        

%%--------------------------------------------------------------------
%% @doc create a new content based on test connector
%% @end
%%--------------------------------------------------------------------
create_contents(Config) ->
    ok.


