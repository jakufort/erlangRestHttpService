%%%-------------------------------------------------------------------
%%% @author Jakub Fortunka
%%% @copyright (C) 2015, Jakub Fortunka
%%% @doc
%%%
%%% @end
%%% Created : 17. sty 2015 22:13
%%%-------------------------------------------------------------------
-module(measurer).
-author("fortun").

-import(httpc,[request/4]).
-import(random,[uniform/1]).
-import(math,[pow/2]).
-import(base64,[encode_to_string/1]).
-import(lists,[append/1]).

%% API
-export([init/0,startSending/2, sendLoop/2,addMeasurerToServer/2,deleteMeasurer/3,sendMeasure/2, sendMeasureJson/2  ]).

init() ->
  inets:start().

addMeasurerToServer(Name,Password) ->
  makeRequest(post,"http://localhost:8000/testSite/api/measures/",[],"application/x-www-form-urlencoded",
    "name=admin&password=admin&measurer="++Name++"&measurerPassword="++Password,[],[]).

deleteMeasurer(Username,Password,MeasurerName) ->
  makeRequest(delete,"http://localhost:8000/testSite/api/measures/"++MeasurerName,
    [auth_header(Username,Password)],"", "",[],[]).

auth_header(User, Pass) ->
    Encoded = base64:encode_to_string(lists:append([User,":",Pass])),
    {"Authorization","Basic " ++ Encoded}.

startSending(MeasurerName, MeasurerPassword) ->
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
  makeRequest(put,"http://localhost:8000/testSite/api/measures/"++Measurer,
    [],"application/x-www-form-urlencoded", Body,[],[]).

sendMeasureJson(Measurer,Password) ->
  io:format("sending\n"),
  {M,S,_} = now(),
  Timestamp = M * 1000000 + S,
  Body = "{\"password\":\""++Password++"\",\"timestamp\":"++integer_to_list(Timestamp)++",\"value\":"++float_to_list(round(random:uniform()*20,2))++"}",
  makeRequest(put,"http://localhost:8000/testSite/api/measures/"++Measurer,
    [],"application/json",Body,
    [],[]).

round(Number, Precision) ->
    P = math:pow(10, Precision),
    round(Number * P) / P.

makeRequest(Method,URL,Header,Type,Body,HTTPOptions,Options) ->
  httpc:request(Method, {URL, Header, Type, Body}, HTTPOptions, Options).
