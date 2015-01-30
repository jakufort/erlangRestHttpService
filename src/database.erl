%%%-------------------------------------------------------------------
%%% @author fortun
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. sty 2015 10:54
%%%-------------------------------------------------------------------
-module(database).
-author("fortun").
-include("headers.hrl").

%% API
-export([selectAll/1,select/2,selectWithLimit/3,
  deleteTable/1,deleteRecord/2,
  authenticate/3,measurerExists/1,clearDatabase/0,addMeasurer/2,addMeasure/3,replaceMeasurers/1]).


selectAll(Table) ->
  application:start(mnesia),
  F = fun() -> mnesia:select(Table, [{'_', [], ['$_']}]) end,
  mnesia:activity(transaction, F).

select(Table, Index) ->
  application:start(mnesia),
  F = fun() -> mnesia:match_object(Table, {Table, Index, '_'}, read) end,
  mnesia:activity(transaction, F).

selectWithLimit(Table, Limit, Fun) ->
  limitSelect(lists:sort(Fun,selectAll(Table)),Limit).

limitSelect(_,0) -> [];
limitSelect([],_) -> [];
limitSelect([H|T], Limit) ->
  [H]++limitSelect(T,Limit-1).

deleteTable(Table) ->
  mnesia:delete_table(Table).

deleteRecord(Table, Record) ->
  application:start(mnesia),
  F = fun() -> mnesia:delete(Table, Record, write) end,
  mnesia:activity(transaction, F).


%%%% MEASURERS

authenticate(Table, Name, Password) ->
  case database:select(Table, Name) of
    [{Table, _, Pass}] ->
      string:equal(Pass, Password);
    _ -> false
  end.

measurerExists(Measurer) ->
  case database:select(measurers, Measurer) of
    [{measurers, _, _}] -> true;
    _ -> false
  end.

clearDatabase() ->
  Measurers = database:selectAll(measurers),
  lists:foreach(fun({_,Name,_}) -> database:deleteTable(list_to_atom(Name)) end, Measurers),
  application:start(mnesia),
  mnesia:clear_table(measurers).

addMeasurer(Name, Password) ->
  application:start(mnesia),
  Nodes = lists:foldr(fun({_,Node,_},Acc) ->  [Node] ++ Acc end,[],database:selectAll(dbNodes)),
  mnesia:create_table(list_to_atom(Name),
    [{attributes, record_info(fields, measures)},
      {disc_copies, Nodes},
      {record_name,measures}]),

  Fun = fun() ->
    mnesia:write(#measurers{name = Name, password = Password})
  end,
  mnesia:transaction(Fun),
  [{status,201},{html,Name}].

addMeasure(MeasurerName,Timestamp,Measure) ->
  application:start(mnesia),
  Fun = fun() ->
    mnesia:write(MeasurerName, #measures{timestamp = Timestamp, measure = Measure}, write)
  end,
  mnesia:activity(transaction,Fun).

replaceMeasurers(Data) ->
  %%removing all measures and measurers
  clearDatabase(),
  lists:foreach(
    fun({[{_,N},{_,Password},{_,Measures}]}) ->
      Name = binary_to_list(N),
      addMeasurer(Name,binary_to_list(Password)),
      lists:foreach(fun({[{_,Timestamp},{_,Value}]}) ->
        addMeasure(list_to_atom(Name),Timestamp,Value)
      end,Measures)
    end,Data).