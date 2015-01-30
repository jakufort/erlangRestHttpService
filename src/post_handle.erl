%%%-------------------------------------------------------------------
%%% @author fortun
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. sty 2015 10:47
%%%-------------------------------------------------------------------
-module(post_handle).
-author("fortun").

-include("/home/fortun/git/yaws/include/yaws_api.hrl").

%% API
-export([handle/2]).

handle(Arg, []) ->
  case yaws_api:parse_post(Arg) of
    [{"name", Username}, {"password", Password}, {"measurer", MeasurerName}, {"measurerPassword", MeasurerPassword}] ->
      case database:authenticate(admins, Username, Password) of
        false ->
          utils:badPassword();
        true ->
          database:addMeasurer(MeasurerName, MeasurerPassword)
      end;
    _ ->
      {status,400}
  end;

handle(_, _) ->
  utils:reactToWrongWay().

