%%%-------------------------------------------------------------------
%%% @author fortun
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. sty 2015 11:37
%%%-------------------------------------------------------------------
-module(measurer_utils).
-author("fortun").

-import(httpc,[request/4]).
-import(math,[pow/2]).
-import(random,[uniform/0]).

%% API
-export([startSending/2,sendLoop/2,sendMeasureJson/2]).

startSending(MeasurerName, MeasurerPassword) ->
  inets:start(),
  Pid = spawn(?MODULE,sendLoop,[MeasurerName,MeasurerPassword]),
  {ok,Pid}.

sendLoop(MeasurerName,MeasurerPassword) ->
  sendMeasureJson(MeasurerName,MeasurerPassword),
  receive
    {stop} -> exit(stopped)
  after
    1000*30 -> sendLoop(MeasurerName,MeasurerPassword)
  end.

sendMeasure(Measurer,Password) ->
  io:format("sending\n"),
  {M,S,_} = now(),
  Timestamp = M * 1000000 + S,
  Body = "password="++Password++"&timestamp="++integer_to_list(Timestamp)++"&value="++float_to_list(round(random:uniform()*20,2)),
  httpc:request(put,{"http://localhost:8000/testSite/api/measures/"++Measurer,
    [],"application/x-www-form-urlencoded", Body},[],[]).

sendMeasureJson(Measurer,Password) ->
  io:format("sending\n"),
  {M,S,_} = now(),
  Timestamp = M * 1000000 + S,
  Body = "{\"password\":\""++Password++"\",\"timestamp\":"++integer_to_list(Timestamp)
    ++",\"value\":"++float_to_list(round(random:uniform()*20,2))++"}",
  httpc:request(put,{"http://localhost:8000/testSite/api/measures/"++Measurer,
    [],"application/json",Body},
    [],[]).

round(Number, Precision) ->
  P = math:pow(10, Precision),
  round(Number * P) / P.