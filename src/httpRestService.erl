-module(httpRestService).
-include("/home/fortun/git/yaws/include/yaws_api.hrl").
%-include("jiffy.app").

-export([handle/2, out/1, initialize/2, addMeasurer/2, initialize/3]).

-import(lists, [foreach/2, foldr/3]).
-import(string, [equal/2,to_integer/1]).

-record(admins, {login, password}).

-record(measurers, {name, password}).

-record(measures, {timestamp, measure}).

-record(dbNodes,{name, empty}).

%-record(nodes,{index,nodeName}).


initialize(AdminName, AdminPassword) ->
  createSchema(AdminName, AdminPassword, []).

initialize(AdminName, AdminPassword, Nodes) ->
  createSchema(AdminName,AdminPassword,Nodes).

createSchema(AdminName, AdminPassword, Nodes_) ->
  application:start(mnesia),
  Nodes = lists:append([Nodes_,[node()]]),

  mnesia:create_schema(Nodes),
  mnesia:change_config(extra_db_nodes,Nodes),
  foreach(fun(Node) -> mnesia:change_table_copy_type(schema,Node,disc_copies) end,Nodes),

  %% Creating table with measurers (used to authenticate)
  mnesia:create_table(measurers,
    [{attributes, record_info(fields, measurers)},
      {disc_copies, Nodes}
    ]),

  %% Creating table with admins (used to authenticate)
  mnesia:create_table(admins,
    [{attributes, record_info(fields, admins)},
      {disc_copies, Nodes}
    ]),

  mnesia:create_table(dbNodes,
    [{attributes, record_info(fields,dbNodes)},
      {disc_copies,Nodes}
    ]),

  %%TODO check if it would work

  lists:foreach(fun(Node) ->
    F = fun() -> mnesia:write(#dbNodes{name = Node, empty = ""}) end,
    mnesia:transaction(F)
  end,Nodes),

  Fun = fun() -> mnesia:write(#admins{login = AdminName, password = AdminPassword}) end,
  mnesia:transaction(Fun),

  application:stop(mnesia).

out(Arg) ->
  case Arg#arg.pathinfo of
    "/" -> {html, "no!"};
    undefined -> {html, "no!"};
    _ ->
      handle(method(Arg), Arg)
  end.

method(Arg) ->
  Rec = Arg#arg.req,
  Rec#http_request.method.

handle(Method, Arg) ->
  [Type | T] = string:tokens(Arg#arg.pathinfo, "/"),
  case Type of
    "measures" -> handle(Method, Arg, T);
    _ ->
      reactToWrongWay()
  end.

reactToWrongWay() ->
  {status,404}.


%%%%%%%%%%%%%%%
%%%%  GET  %%%%
%%%%%%%%%%%%%%%

handle('GET', _, []) ->
  %list all measurers
  returnJson(
    makeJson(
      selectAll(measurers),
      fun({_,Name,_},Acc) -> [list_to_binary(Name)] ++ [list_to_binary(Acc)] end));

handle('GET', Arg, [MeasurerName]) ->
  case measurerExists(MeasurerName) of
    true ->
      case yaws_api:parse_query(Arg) of
        [] ->
          returnJson(
            makeJson(
              selectAll(list_to_atom(MeasurerName)),
              fun({_,Timestamp,Measure}, Acc) -> [{[{timestamp,Timestamp},{value,Measure}]}] ++ Acc end));
        [{"limit", Limit}] ->
          case string:to_integer(Limit) of
            {L,[]} ->
              returnJson(
              makeJson(
                selectWithLimit(list_to_atom(MeasurerName),L,fun({_,A,_},{_,B,_}) -> A>B end),
                fun({_,Timestamp,Measure}, Acc) -> [{[{timestamp,Timestamp},{value,Measure}]}] ++ Acc end));

            {error,_} -> {status,400};
            _ -> {status,400}
          end;
        _ -> {status, 400}
      end;
    false ->
      reactToWrongWay()
  end;

handle('GET', _, _) ->
  reactToWrongWay();


%%%%%%%%%%%%%%
%%%% POST %%%%
%%%%%%%%%%%%%%

handle('POST', Arg, []) ->
  case yaws_api:parse_post(Arg) of
    [{"name", Username}, {"password", Password}, {"measurer", MeasurerName}, {"measurerPassword", MeasurerPassword}] ->
      case authenticate(admins, Username, Password) of
        false ->
          badPassword();
        true ->
          addMeasurer(MeasurerName, MeasurerPassword)
      end;
    _ ->
      {status,400}
  end;

handle('POST', _, _) ->
  reactToWrongWay();


%%%%%%%%%%%%%%
%%%% PUT %%%%%
%%%%%%%%%%%%%%

handle('PUT', Arg, []) ->
  case Arg#arg.headers#headers.authorization of
    {Name,Password,_} ->
      case authenticate(admins,Name,Password) of
        false ->
          badPassword();
        true ->
          {[{<<"measurers">>,Data}]} = jiffy:decode(Arg#arg.clidata),
          replaceMeasurers(Data),
          {status, 201}
      end;
    _ ->
      [{status,401},{header, "WWW-Authenticate: Basic realm=\"REST API\""}]
  end;

handle('PUT', Arg, [MeasurerName]) ->
  case measurerExists(MeasurerName) of
    true ->
      case Arg#arg.headers#headers.content_type of
        "application/x-www-form-urlencoded" ->
          [{"password",Password},{"timestamp",Timestamp},{"value",Value}] = yaws_api:parse_query( Arg#arg{ querydata = Arg#arg.clidata } ),
          handleAddingMeasure(MeasurerName,Password,Timestamp,Value);
        "application/json" ->
          {[{_,Password},{_,Timestamp},{_,Value}]} = jiffy:decode(Arg#arg.clidata),
          handleAddingMeasure(MeasurerName,binary_to_list(Password),integer_to_list(Timestamp),float_to_list(Value))
      end;
    false ->
      reactToWrongWay()
  end;

handle('PUT', _, _) ->
  reactToWrongWay();


%%%%%%%%%%%%%%
%%% DELETE %%%
%%%%%%%%%%%%%%

handle('DELETE', Arg, T) ->
  case Arg#arg.headers#headers.authorization of
    {Login,Password,_} ->
      case authenticate(admins,Login,Password) of
        false ->
          badPassword();
        true ->
          handleDel(Arg,T)
      end;
    _ ->
      [{status,401},{header, "WWW-Authenticate: Basic realm=\"REST API\""}]
  end;

%%%%%%%%%%%%%%%
%%% OPTIONS %%%
%%%%%%%%%%%%%%%

handle('OPTIONS', _, []) ->
  {header, "Allow: GET,PUT,POST,DELETE"};

handle('OPTIONS',_,[MeasurerName]) ->
  case measurerExists(MeasurerName) of
    true ->
      {header, "Allow: GET,PUT,DELETE"};
    _ ->
      {header, "Allow:"}
  end;

%%%%%%%%%%%%%%%
%%% UNKNOWN %%%
%%%%%%%%%%%%%%%

handle(_, _, _) ->
  {html, "unknown"}.

badPassword() ->
  {status,401}.

%%% Handle DELETE after Authentication

handleDel(_, []) ->
  Measurers = selectAll(measurers),
  lists:foreach(fun({_,Name,_}) -> deleteTable(list_to_atom(Name)) end, Measurers),
  application:start(mnesia),
  mnesia:clear_table(measurers),
  {status, 200};

handleDel(_, [MeasurerName]) ->
  case measurerExists(MeasurerName) of
    true ->
      deleteTable(list_to_atom(MeasurerName)),
      deleteRecord(measurers, MeasurerName),
      {status, 200};
    false ->
      reactToWrongWay()
  end;

handleDel(_, _) ->
  reactToWrongWay().

%% JSON handling

makeJson(L,Fun) ->
  List = lists:foldr(Fun, [], L),
  jiffy:encode(List).

returnJson(Json) ->
  {content,
    "application/json; charset=iso-8859-1",
    Json}.

%%% admin

authenticate(Table, Name, Password) ->
  case select(Table, Name) of
    [{Table, _, Pass}] ->
      string:equal(Pass, Password);
    _ -> false
  end.

%%%   Measure

handleAddingMeasure(Name,Password,Timestamp,Value) ->
  case authenticate(measurers, Name, Password) of
    false ->
      badPassword();
    true ->
      try addMeasure(list_to_atom(Name),list_to_integer(Timestamp),list_to_float(Value)) of
        R -> R
      catch
        badarg -> {status,400}
      end
  end.

replaceMeasurers(Data) ->
  %%removing all measures and measurers
  handleDel(a,[]),
  lists:foreach(
    fun({[{_,N},{_,Password},{_,Measures}]}) ->
      Name = binary_to_list(N),
      addMeasurer(Name,binary_to_list(Password)),
      lists:foreach(fun({[{_,Timestamp},{_,Value}]}) ->
        addMeasure(list_to_atom(Name),Timestamp,Value)
      end,Measures)
    end,Data).

addMeasurer(Name, Password) ->
  application:start(mnesia),
  Nodes = lists:foldr(fun({_,Node,_},Acc) ->  [Node] ++ Acc end,[],selectAll(dbNodes)),
  mnesia:create_table(list_to_atom(Name),
    [{attributes, record_info(fields, measures)},
     {disc_copies, Nodes},
      {record_name,measures}]),

  Fun = fun() ->
    mnesia:write(#measurers{name = Name, password = Password})
  end,
  mnesia:transaction(Fun),
  [{status,201},{html,Name}].

measurerExists(Measurer) ->
  case select(measurers, Measurer) of
    [{measurers, _, _}] -> true;
    _ -> false
  end.

addMeasure(MeasurerName,Timestamp,Measure) ->
  application:start(mnesia),
  Fun = fun() ->
    mnesia:write(MeasurerName, #measures{timestamp = Timestamp, measure = Measure}, write)
  end,
  mnesia:activity(transaction,Fun).

%%% mnesia

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
  %application:stop(mnesia).

deleteRecord(Table, Record) ->
  application:start(mnesia),
  F = fun() -> mnesia:delete(Table, Record, write) end,
  mnesia:activity(transaction, F).
  %application:stop(mnesia).