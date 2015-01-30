%%%-------------------------------------------------------------------
%%% @author fortun
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. sty 2015 10:48
%%%-------------------------------------------------------------------
-module(delete_handle).
-author("fortun").

-include("/home/fortun/git/yaws/include/yaws_api.hrl").

%% API
-export([handle/2]).

handle(Arg, T) ->
  case Arg#arg.headers#headers.authorization of
    {Login,Password,_} ->
      case database:authenticate(admins,Login,Password) of
        false ->
          utils:badPassword();
        true ->
          handleDel(Arg,T)
      end;
    _ ->
      [{status,401},{header, "WWW-Authenticate: Basic realm=\"REST API\""}]
  end.


%%% Handle DELETE after Authentication

handleDel(_, []) ->
  database:clearDatabase(),
  {status, 200};

handleDel(_, [MeasurerName]) ->
  case database:measurerExists(MeasurerName) of
    true ->
      database:deleteTable(list_to_atom(MeasurerName)),
      database:deleteRecord(measurers, MeasurerName),
      {status, 200};
    false ->
      utils:reactToWrongWay()
  end;

handleDel(_, _) ->
  utils:reactToWrongWay().
