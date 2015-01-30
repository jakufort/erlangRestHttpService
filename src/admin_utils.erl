%%%-------------------------------------------------------------------
%%% @author fortun
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. sty 2015 11:33
%%%-------------------------------------------------------------------
-module(admin_utils).
-author("fortun").

-import(httpc,[request/4]).
-import(base64,[encode_to_string/1]).

%% API
-export([addMeasurer/4,deleteMeasurer/3]).

addMeasurer(Login,AdminPassword,Name,Password) ->
  inets:start(),
  httpc:request(post,{"http://localhost:8000/testSite/api/measures/",[],"application/x-www-form-urlencoded",
    "name="++Login++"&password="++AdminPassword++"&measurer="++Name++"&measurerPassword="++Password},[],[]).

deleteMeasurer(Username,Password,MeasurerName) ->
  inets:start(),
  httpc:request(delete,{"http://localhost:8000/testSite/api/measures/"++MeasurerName,
    [auth_header(Username,Password)],"", ""},[],[]).

auth_header(User, Pass) ->
  Encoded = base64:encode_to_string(lists:append([User,":",Pass])),
  {"Authorization","Basic " ++ Encoded}.
