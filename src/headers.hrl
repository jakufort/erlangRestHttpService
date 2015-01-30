%%%-------------------------------------------------------------------
%%% @author fortun
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. sty 2015 10:54
%%%-------------------------------------------------------------------
-author("fortun").

-record(admins, {login, password}).

-record(measurers, {name, password}).

-record(measures, {timestamp, measure}).

-record(dbNodes,{name, empty}).
