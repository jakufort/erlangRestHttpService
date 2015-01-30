%%%-------------------------------------------------------------------
%%% @author fortun
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. sty 2015 11:06
%%%-------------------------------------------------------------------
-module(options_handle).
-author("fortun").

%% API
-export([handle/1]).

handle([]) ->
  {header, "Allow: GET,PUT,POST,DELETE"};

handle([MeasurerName]) ->
  case database:measurerExists(MeasurerName) of
    true ->
      {header, "Allow: GET,PUT,DELETE"};
    _ ->
      {header, "Allow:"}
  end.