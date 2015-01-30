%%%-------------------------------------------------------------------
%%% @author fortun
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. sty 2015 10:57
%%%-------------------------------------------------------------------
-module(utils).
-author("fortun").
-include("headers.hrl").

%% API
-export([badPassword/0,reactToWrongWay/0,makeJson/2,returnJson/1,handleAddingMeasure/4]).

badPassword() ->
  {status,401}.

reactToWrongWay() ->
  {status,404}.

%%%   JSON

makeJson(L,Fun) ->
  List = lists:foldr(Fun, [], L),
  jiffy:encode(List).

returnJson(Json) ->
  {content,
    "application/json; charset=iso-8859-1",
    Json}.

%%%   Measure

handleAddingMeasure(Name,Password,Timestamp,Value) ->
  case database:authenticate(measurers, Name, Password) of
    false ->
      badPassword();
    true ->
      try database:addMeasure(list_to_atom(Name),list_to_integer(Timestamp),list_to_float(Value)) of
        R -> R
      catch
        badarg -> {status,400}
      end
  end.

