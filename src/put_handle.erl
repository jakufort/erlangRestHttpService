%%%-------------------------------------------------------------------
%%% @author fortun
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. sty 2015 10:45
%%%-------------------------------------------------------------------
-module(put_handle).
-author("fortun").

-include("/home/fortun/git/yaws/include/yaws_api.hrl").

%% API
-export([handle/2]).

handle(Arg, []) ->
  case Arg#arg.headers#headers.authorization of
    {Name,Password,_} ->
      case database:authenticate(admins,Name,Password) of
        false ->
          utils:badPassword();
        true ->
          {[{<<"measurers">>,Data}]} = jiffy:decode(Arg#arg.clidata),
          database:replaceMeasurers(Data),
          {status, 201}
      end;
    _ ->
      [{status,401},{header, "WWW-Authenticate: Basic realm=\"REST API\""}]
  end;

handle(Arg, [MeasurerName]) ->
  case database:measurerExists(MeasurerName) of
    true ->
      case Arg#arg.headers#headers.content_type of
        "application/x-www-form-urlencoded" ->
          [{"password",Password},{"timestamp",Timestamp},{"value",Value}] = yaws_api:parse_query( Arg#arg{ querydata = Arg#arg.clidata } ),
          utils:handleAddingMeasure(MeasurerName,Password,Timestamp,Value);
        "application/json" ->
          {[{_,Password},{_,Timestamp},{_,Value}]} = jiffy:decode(Arg#arg.clidata),
          utils:handleAddingMeasure(MeasurerName,binary_to_list(Password),integer_to_list(Timestamp),float_to_list(Value))
      end;
    false ->
      utils:reactToWrongWay()
  end;

handle(_, _) ->
  utils:reactToWrongWay().
