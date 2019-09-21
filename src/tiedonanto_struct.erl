%%%-------------------------------------------------------------------
%%% @author Mathieu Kerjouan <contact@steepath.eu>
%%% @copyright 2019 Mathieu Kerjouan
%%%
%%% @doc tiedonanto_struct create an internal data-structure mapping
%%%      and storing all information required for the different
%%%      end-point. So, what is an end-point? this structure should
%%%      define it clearly.
%%%
%%%      Firstly, an endpoint is a local or remote (e.g. tcp, udp,
%%%      http) end-point. This structure must embedded all information
%%%      to create the link between our local actor/ports.
%%%
%%%      Secondly, not all endpoint are the same and are not ruled 
%%%      the same too. The structure should embed the way to 
%%%      communicate, create, alter or remove data.
%%%
%%%      Third, the structure should have a payload, with raw data
%%%      and associated meta-data. this payload should be also tightly 
%%%      linked to the rules.
%%% @end
%%%
%%%-------------------------------------------------------------------
-module(tiedonanto_struct).
-export([new/0, new/1]).
-export([rules/1]).
-export([endpoint/1]).
-export([payload/1]).
-include("tiedonanto_struct.hrl").
-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec new() -> tiedonanto_struct().
new() ->
    #{ rules => new(rules) 
     , endpoint => new(endpoint) 
     , payload => new(payload) 
     }.

new0_test() ->
    ok.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec new(rules) -> tiedonanto_rules();
         (endpoint) -> tiedonanto_endpoint();
         (payload) -> tiedonanto_payload().
new(rules) ->
    #{};
new(endpoint) ->
    #{};
new(payload) ->
    #{}.

new1_test() ->
    ?assertEqual(new(rules), #{}),
    ?assertEqual(new(endpoint), #{}),
    ?assertEqual(new(payload), #{}).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec rules(Struct :: tiedonanto_struct()) 
           -> {ok, tiedonanto_rules(), tiedonanto_struct()}.
rules(#{ rules := Rules} = Struct) ->
    {ok, Rules, Struct}.

rules1_test() ->
    IN = new(),
    OUT = {ok, #{}, IN},
    ?assertEqual(rules(IN), OUT).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec endpoint(Struct :: tiedonanto_struct())
              -> {ok, tiedonanto_endpoint(), tiedonanto_struct()}.
endpoint(#{ endpoint := Endpoint} = Struct) ->
    {ok, Endpoint, Struct}.

endpoint1_test() ->
    IN = new(),
    OUT = {ok, #{}, IN},
    ?assertEqual(endpoint(IN), OUT).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
-spec payload(Struct :: tiedonanto_struct())
             -> {ok, tiedonanto_payload(), tiedonanto_struct()}.
payload(#{ payload := Payload} = Struct) ->
    {ok, Payload, Struct}.

payload1_test() ->
    IN = new(),
    OUT = {ok, #{}, IN},
    ?assertEqual(payload(IN), OUT).
