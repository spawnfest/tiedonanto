%%%-------------------------------------------------------------------
%%% @author Mathieu Kerjouan <contact@steepath.eu>
%%% @copyright 2019 Mathieu Kerjouan
%%%
%%% @doc tiedonanto module contains all function to manage the 
%%%      application. It gives you the possibility to create new 
%%%      controllers, rules, contents and pipelines but also give
%%%      you all required information to manage generated content.
%%% @end
%%%-------------------------------------------------------------------
-module(tiedonanto).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------
%% @doc controller/1 create a new controller with default argument.
%% @end
%%--------------------------------------------------------------------
-spec controller(Name :: atom()) 
               -> {ok, atom(), list()}.
controller(Name) ->
    controller(Name, []).

%%--------------------------------------------------------------------
%% @doc controller/2 creates a new controller with configurable 
%%      parameter.
%% @end
%%--------------------------------------------------------------------
-spec controller(Name :: atom(), Opts :: list()) 
               -> {ok, atom(), list()}.
controller(Name, Opts) ->
    tiedonanto_controller:controller(Name, Opts).

%%--------------------------------------------------------------------
%% @doc controllers/0 list all available controllers.
%% @end
%%--------------------------------------------------------------------
-spec controllers() -> [atom(), ...].
controllers() ->
    tiedonanto_controller:controllers().

%%--------------------------------------------------------------------
%% @doc rule/2 create a new rule into a controller, by default, rule/2
%%      only allocate the rule but does not create any content in it.
%% @end
%%--------------------------------------------------------------------
-spec rule(Controller :: atom(), Name :: atom()) 
          -> {ok, atom(), list()}.
rule(Controller, Name) ->
    rule(Controller, Name, []).

%%--------------------------------------------------------------------
%% @doc rule/3 create a new rule into a controller by allocating
%%      it and adding all parameters.
%% @end
%%--------------------------------------------------------------------
-spec rule(Controller :: atom(), Name :: atom(), Content :: list()) 
          -> {ok, atom(), list()}.
rule(Controller, Name, Content) ->
    ok.

%%--------------------------------------------------------------------
%% @doc rules/1 list all available rules from a controller.
%% @end
%%--------------------------------------------------------------------
-spec rules(Controller :: atom()) -> {ok, list()}.
rules(Controller) ->
    ok.

%%--------------------------------------------------------------------
%% @doc content/1
%% @end
%%--------------------------------------------------------------------
-spec content(Content :: bitstring() | string()) -> {ok, reference()}.
content(Content) ->
    content(Content, []).

%%--------------------------------------------------------------------
%% @doc content/2
%% @end
%%--------------------------------------------------------------------
-spec content( Content :: bitstring() | string()
             , Controllers :: [atom(), ...]) 
             -> {ok, reference()}.
content(Content, Controllers) ->
    content(Content, Controllers, []).

%%--------------------------------------------------------------------
%% @doc content/3
%% @end
%%--------------------------------------------------------------------
-spec content( Content :: bitstring() | string()
             , Controllers :: [atom(), ...]
             , Opts :: list())
             -> {ok, reference()}.
content(Content, Controllers, Opts) ->
    {ok, erlang:make_ref()}.

%%--------------------------------------------------------------------
%% @doc contents/1
%% @end
%%--------------------------------------------------------------------
-spec contents(Controller :: atom()) -> {ok, list()}.
contents(Controller) ->
    contents(Controller, []).

-spec contents(Controller :: atom(), Opts :: list()) 
              -> {ok, list()}.
contents(Controller, Opts) ->
    ok.
                      
