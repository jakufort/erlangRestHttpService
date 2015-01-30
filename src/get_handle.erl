%%%-------------------------------------------------------------------
%%% @author fortun
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. sty 2015 10:46
%%%-------------------------------------------------------------------
-module(get_handle).
-author("fortun").

-include("/home/fortun/git/yaws/include/yaws_api.hrl").

%% API
-export([handle/2]).

handle(_, []) ->
  %list all measurers
  utils:returnJson(
    utils:makeJson(
      database:selectAll(measurers),
      fun({_,Name,_},Acc) -> [list_to_binary(Name)] ++ [list_to_binary(Acc)] end));

handle(Arg, [MeasurerName]) ->
  case database:measurerExists(MeasurerName) of
    true ->
      case yaws_api:parse_query(Arg) of
        [] ->
          utils:returnJson(
            utils:makeJson(
              database:selectAll(list_to_atom(MeasurerName)),
              fun({_,Timestamp,Measure}, Acc) -> [{[{timestamp,Timestamp},{value,Measure}]}] ++ Acc end));
        [{"limit", Limit}] ->
          case string:to_integer(Limit) of
            {L,[]} ->
              utils:returnJson(
                utils:makeJson(
                  database:selectWithLimit(list_to_atom(MeasurerName),L,fun({_,A,_},{_,B,_}) -> A>B end),
                  fun({_,Timestamp,Measure}, Acc) -> [{[{timestamp,Timestamp},{value,Measure}]}] ++ Acc end));

            {error,_} -> {status,400};
            _ -> {status,400}
          end;
        _ -> {status, 400}
      end;
    false ->
      utils:reactToWrongWay()
  end;

handle(_, _) ->
  utils:reactToWrongWay().
