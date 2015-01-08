-module(httpRestService).
-include("/home/fortun/git/yaws/include/yaws_api.hrl").
-export([method/1,handle/2]).

-record(nodes,{measurerName,nodeName,pid}).
-record(admins,{login,password}).

%TODO init admins database?

method(Arg) ->
    Rec = Arg#arg.req,
    Rec#http_request.method.

%returns list with url tokens
getPath(Arg) ->
    Uri = yaws_api:request_url(Arg),
    Uri#url.path.

handle(Method,Arg) ->
    [_,Type|T] = string:tokens(getPath(Arg),"/"),
    case Type of
        "measures" -> handle(Method,Arg,measures,T);
        "admin" -> handle(Method,Arg,admin,T)
    end.

handle('GET',Arg,measures,RestOfPath) ->
    {html,"mes"};

handle('GET',Arg,admin,RestOfPath) ->
    {html,"adm"};

handle('POST',Arg,measures,_) ->
    Fun = fun() ->
            [Name,Password,Measure,Time] = yaws_api:parse_post(Arg),
            Row = select(nodes,Name),
            Row#nodes.pid ! {Name,Password,Measure,Time}
        end,
    spawn(Fun),
    {html,"Done"};

handle('POST',Arg,admin,RestOfPath) ->
    [Username,Password|T] = yaws_api:parse_post(Arg),
    case checkCredentials(Username,Password) of
        false ->
            {html,"error"};
        true ->
            handleAdmin('POST',Arg,T,RestOfPath),
            {html,"success"}
    end;

handle('PUT',Arg,measures,_) ->
    {html,"Put"};

handle('DELETE',Arg,measures,_) ->
    {html,"Delete"};

handle(Method,_,_,_) ->
    {html,"Unknown"}.

handleAdmin('POST', Arg, RestOfPost,RestOfPath) ->
    [Tmp|_] = RestOfPath,
    case Tmp of
        "measurers" ->
            {html,"measurers"};
        "nodes" ->
            {html,"nodes"};
        "users" ->
            {html,"users"}
    end.

%%% measures



%%% admin

checkCredentials(Username,Password) ->
    case select(admins,Username) of
        {_,Pass} ->
            Pass = Password;
        _ -> false
    end.

%%% mnesia

select(Table,Index) ->
    mnesia:start(),
    Fun = fun() ->
        mnesia:read({Table,Index})
    end,
    {atomic,[Row]} = mnesia:transaction(Fun),
    mnesia:stop(),
    Row.