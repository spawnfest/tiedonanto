%%%-------------------------------------------------------------------
%%% @author Mathieu Kerjouan <contact@steepath.eu>
%%% @copyright 2019 Mathieu Kerjouan
%%%
%%% @doc tiedonanto_controller is the sub-application controller, used
%%%      to manage dynamic end-point and static one.
%%% @end
%%%-------------------------------------------------------------------
-module(tiedonanto_controller).
-export([start_link/0, start_link/1, start_link/2]).
-export([stop/0]).
-export([init/1, terminate/2]).
-export([handle_call/3, handle_cast/2]).
-export([controller/1, controller/2, controllers/0]).
-export([rule/2, rule/3, rules/0]).
-behavior(gen_server).
-record(state, { controllers_db :: reference()
               , rules_db :: reference()
               }).

%%--------------------------------------------------------------------
%% @doc start_link/0
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> {ok, pid()}.
start_link() ->
    start_link([]).

%%--------------------------------------------------------------------
%% @doc start_link/1
%% @end
%%--------------------------------------------------------------------
-spec start_link(Args :: list()) -> {ok, pid()}.
start_link(Args) ->
    start_link(Args, []).

%%--------------------------------------------------------------------
%% @doc start_link/2
%% @end
%%--------------------------------------------------------------------
-spec start_link(Args :: list(), Opts :: list()) -> {ok, pid()}.
start_link(Args, Opts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, Opts).

%%--------------------------------------------------------------------
%% @doc stop/0
%% @end
%%--------------------------------------------------------------------
-spec stop() -> ok.
stop() ->
    gen_server:stop(?MODULE).

%%--------------------------------------------------------------------
%% @doc init/1
%% @end
%%--------------------------------------------------------------------
-spec init(Args :: list()) -> {ok, #state{}}.
init(_Args) ->
    State = #state{ controllers_db = ets:new(?MODULE, []) 
                  , rules_db = ets:new(?MODULE, [])
                  },
    logger:debug("create ets ~p", [State]),
    {ok, State}.

%%--------------------------------------------------------------------
%% @doc terminate/2
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason :: term(), State :: #state{}) -> ok.
terminate(_Reason, State) ->
    logger:warning("state ~p will be lost!", [State]),
    ok.

%%--------------------------------------------------------------------
%% @doc handle_call/3
%% @end
%%--------------------------------------------------------------------
-spec handle_call( Data :: term()
                 , From :: term()
                 , State :: #state{}) 
                 -> {reply, term(), #state{}}.
handle_call({create, Create}, _From, State) ->
    handle_create(Create, State);
handle_call({get, Get}, _From, State) ->
    handle_get(Get, State);
handle_call(Data, From, State) ->
    logger:debug("receive ~p from ~p", [Data, From]),
    {reply, Data, State}.

%%--------------------------------------------------------------------
%% @doc handle_cast/2
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(Data :: term(), State :: #state{}) -> {noreply, #state{}}.
handle_cast(Data, State) ->
    logger:debug("receive ~p", [Data]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
handle_create({controller, ControllerName, Opts}
              ,#state{ controllers_db = CDB } = State) ->
    logger:debug("add new controller ~p with options ~p", [ControllerName, Opts]),
    _Result = ets:insert(CDB, {ControllerName, Opts}),
    {reply, {ok, ControllerName, Opts}, State};
handle_create({rule, ControllerName, RuleName, Opts}
             ,#state{ rules_db = RDB } = State) ->
    logger:debug("add new rule ~p with options ~p", [RuleName, Opts]),
    _Result = ets:insert(RDB, {RuleName, ControllerName, Opts}),
    {reply, {ok, {ControllerName, RuleName, Opts}}, State}.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
handle_get({controller, all}
          ,#state{ controllers_db = CDB } = State) ->
    logger:debug("get all state"),
    Result = ets:select(CDB, [{{'$1', '$2'}, [], [{{'$1', '$2'}}]}]),
    {reply, {ok, Result}, State};
handle_get({rule, all}
          ,#state{ rules_db = RDB } = State) ->
    Select = [{{'$1', '$2', '$3'}, [], [{{'$2', '$1', '$3'}}]}],
    Result = ets:select(RDB, Select),
    {reply, {ok, Result}, State}.

%%--------------------------------------------------------------------
%% @doc controller/2 API
%% @end
%%--------------------------------------------------------------------
-spec controller(Controller :: term()) 
                -> {ok, {term(), list()}}.
controller(Controller) ->
    controller(Controller, []).

%%--------------------------------------------------------------------
%% @doc controller/2 API
%% @end
%%--------------------------------------------------------------------
-spec controller(Controller :: term(), Opts :: list()) 
                -> {ok, {term(), list()}}.
controller(Controller, Opts) ->
    Request = {create, {controller, Controller, Opts}},
    gen_server:call(?MODULE, Request).

%%--------------------------------------------------------------------
%% @doc controllers API
%% @end
%%--------------------------------------------------------------------
-spec controllers() -> {ok, list()}.
controllers() ->
    Request = {get, {controller, all}},
    gen_server:call(?MODULE, Request).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
rule(ControllerName, RuleName) ->
    rule(ControllerName, RuleName, []).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
rule(ControllerName, RuleName, Opts) ->
    Request = {create, {rule, ControllerName, RuleName, Opts}},
    gen_server:call(?MODULE, Request).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
rules() ->
    Request = {get, {rule, all}},
    gen_server:call(?MODULE, Request).

    
