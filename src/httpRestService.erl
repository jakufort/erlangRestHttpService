%%%-------------------------------------------------------------------
%%% @author fortun
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. sty 2015 10:54
%%%-------------------------------------------------------------------
-module(httpRestService).
-author("fortun").
-include("/usr/local/lib/yaws/include/yaws_api.hrl").
-include("headers.hrl").

-export([out/1, initialize/2, initialize/3]).

-import(lists, [foreach/2, foldr/3]).
-import(string, [equal/2,to_integer/1]).


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

  lists:foreach(fun(Node) ->
    F = fun() -> mnesia:write(#dbNodes{name = Node, empty = ""}) end,
    mnesia:transaction(F)
  end,Nodes),

  Fun = fun() -> mnesia:write(#admins{login = AdminName, password = AdminPassword}) end,
  mnesia:transaction(Fun),

  application:stop(mnesia).

out(Arg) ->
  case Arg#arg.pathinfo of
    "/" -> utils:reactToWrongWay();
    undefined -> utils:reactToWrongWay();
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
      utils:reactToWrongWay()
  end.

handle(Method,Arg,T) ->
  case Method of
    'GET' -> get_handle:handle(Arg,T);
    'POST' -> post_handle:handle(Arg,T);
    'PUT' -> put_handle:handle(Arg,T);
    'DELETE' -> delete_handle:handle(Arg,T);
    'OPTIONS' -> options_handle:handle(T);
    _ -> handleUnknown()
  end.

%%%%%%%%%%%%%%%
%%% UNKNOWN %%%
%%%%%%%%%%%%%%%

handleUnknown() ->
  {html,"unknown"}.