%%%-------------------------------------------------------------------
%%% @author Mathieu Kerjouan <contact@steepath.eu>
%%% @copyright 2019 Mathieu Kerjouan
%%%
%%% @doc tiedonanto_rules gives function to manage rules.
%%% @end
%%%-------------------------------------------------------------------
-module(tiedonanto_rules).
-include("tiedonanto.hrl").
-compile(export_all).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
-spec new() ->
new(Content, Action) ->
    {ok, C} = content(Content),
    {ok, A} = action(Action),
    #rule{ content = C
         , action = A
         }.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
-spec content(Content :: list()) -> {ok, Content}.
content(Content) 
  when is_list(Content) ->
    {ok, Content}.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
-spec action(Action :: function()) -> {ok, function()} |
                                      {error, term()}.
action(Action)
  when is_function(Action) ->
    case erlang:fun_info(Action, arity) of
        {arity, 2} -> {ok, Action};
        _Else -> {error, not_valid}
    end.



