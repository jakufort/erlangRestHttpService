-module(httpRestService).
-include("/home/fortun/git/yaws/include/yaws_api.hrl").

-export([method/1,handle/2,out/1,initialize/2,addMeasurerFunction/2, select/2]).

-import(lists,[foreach/2,foldr/3]).
-import(string,[equal/2]).

-record(admins,{login,password}).

-record(measurers,{name,password}).

-record(measures,{timestamp,measure}).

%-record(nodes,{index,nodeName}).


initialize(AdminName,AdminPassword) ->
    %ok = mnesia:create_schema(node()),
    %mnesia:create_schema(node()),

    application:start(mnesia),

    %% Creating table with measurers (used to authenticate)
    mnesia:create_table(measurers,
        [{attributes, record_info(fields, measurers)},
            {disc_copies, [node()]}
        ]),

    %% Creating table with admins (used to authenticate)
    mnesia:create_table(admins,
        [{attributes, record_info(fields,admins)},
            {disc_copies,[node()]}
        ]),

    Fun = fun() -> mnesia:write(#admins{login = AdminName,password = AdminPassword}) end,
    mnesia:transaction(Fun),

    application:stop(mnesia).

out(Arg) ->
    %{ehtml,io_lib:format("A#arg.appmoddata = ~p~n",[Arg#arg.appmoddata])}.
    %{html,"hi"}.
    case Arg#arg.pathinfo of
        "/" -> {html,"no!"};
        undefined -> {html,"no!"};
        _ ->
            handle(method(Arg),Arg)
    end.

method(Arg) ->
    Rec = Arg#arg.req,
    Rec#http_request.method.

handle(Method,Arg) ->
    [Type|T] = string:tokens(Arg#arg.pathinfo,"/"),
    case Type of
        "measures" -> handle(Method,Arg,T);
        _ -> %%TODO react to api/something that is not measures
            {html,"Nothing"}
    end.


%%%%%%%%%%%%%%%
%%%%  GET  %%%%
%%%%%%%%%%%%%%%

%%%%    MEASURES

handle('GET',_,[]) ->
    %list all measurers
    List = select_all(measurers),
    Str = lists:foldr(fun(X,Acc) -> X ++ "<br/>" ++ Acc end,"",List),
    {html,Str};

handle('GET',Arg,[MeasurerName]) ->
    case measurerExists(MeasurerName) of
        true ->
            case yaws_api:parse_query(Arg) of
                [] ->
                    Measures = select_all(MeasurerName),
                    Str = lists:foldr(fun(X,Acc) -> X ++ "<br/>" ++ Acc end,"",Measures),
                    {html,Str};
                [{"limit",Limit}] ->
                    Measures = selectWithLimit(MeasurerName,Limit),
                    Str = lists:foldr(fun(X,Acc) -> X ++ "<br/>" ++ Acc end,"",Measures),
                    {html,Str};
                _ -> {html,"error"}
            end;
        false ->
            {html,"wrong way"}
    end;
    %%TODO send all measures (JSON? XML?)

handle('GET',_,_) ->
    {html,"wrong way"};

%%%%%%%%%%%%%%
%%%% POST %%%%
%%%%%%%%%%%%%%

handle('POST',Arg,[]) ->
    %
%%     Fun = fun() ->
%%             [Name,Password,Measure,Time] = yaws_api:parse_post(Arg),
%%             Row = select(nodes,Name),
%%             Row#nodes.pid ! {Name,Password,Measure,Time}
%%         end,
%%     spawn(Fun),
    %% TODO add new measurer to database - post should contain username and password so it can be authenticated and then executed
%%     F = fun(Parent) ->
%%         case yaws_api:parse_post(Arg) of
%%             [{"name",Username},{"password",Password},{"measurer",MeasurerName},{"measurerPassword",MeasurerPassword}] ->
%%                 case authenticate(admins,Username,Password) of
%%                     false ->
%%                         %% TODO
%%                         Parent ! {self(),{html,"error"}};
%%                     true ->
%%                         addMeasurer(MeasurerName,MeasurerPassword),
%%                         %% TODO return new url (measures/MeasurerName)
%%                         Parent ! {self(),{html,"success"}}
%%                 end;
%%             _ -> Parent ! {self(),{status,400}}
%%         end
%%     end,

    Pid = spawn(?MODULE,addMeasurerFunction,[Arg,self()]),
    receive
        {Pid,Res} -> Res
    end;

handle('POST',_,[MeasurerName]) ->
    %% TODO what should be done here?
    {html,"smth"};

handle('POST',_,_) ->
    {html,"wrong way"};


%%%%%%%%%%%%%%
%%%% PUT %%%%%
%%%%%%%%%%%%%%

handle('PUT',Arg,[]) ->
    %% TODO replace measurers with new "collection" of measurers
    {html,"Put"};

handle('PUT',Arg,[MeasurerName]) ->
    %{M,S,MS} = now(),
    %M * 1000000 * 1000000 + S * 1000000 + MS,
    case measurerExists(MeasurerName) of
        true ->
            [{"password",Pass},{"timestamp",Timestamp},{"measure",Measure}] = yaws_api:parse_query(Arg),

            case authenticate(measurers,MeasurerName,Pass) of
                false ->
                    %% TODO
                    {status,404};
                true ->
                    Fun = fun() ->
                        mnesia:write(MeasurerName, #measures{ timestamp = Timestamp, measure = Measure}, write)
                    end,
                    mnesia:transaction(Fun),
                    {html,"ok"}
            end;
        false ->
            {html,"wrong way"}
    end;

handle('PUT',_,_) ->
    {html,"wrong way"};


%%%%%%%%%%%%%%
%%% DELETE %%%
%%%%%%%%%%%%%%

handle('DELETE',Arg,T) ->
    [{"name",Name},{"password",Password}] = yaws_api:parse_query(Arg),
    case authenticate(admins,Name,Password) of
        false ->
            %% TODO
            {status,400};
        true ->
            handleDel(Arg,T)
    end;

%%%%%%%%%%%%%%%
%%% OPTIONS %%%
%%%%%%%%%%%%%%%

handle('OPTIONS',_,_) ->
    {html,"Options:<br/> GET <br/> PUT <br/> POST <br/> DELETE"};


%%%%%%%%%%%%%%%
%%% UNKNOWN %%%
%%%%%%%%%%%%%%%

handle(_,_,_) ->
    {html,"unknown"}.


%%% Handle DELETE after Authentication

handleDel(_,[]) ->
    Measurers = select_all(measurers),
    lists:foreach(fun(X) -> deleteTable(X) end,Measurers),
    mnesia:clear_table(measurers),
    {html,"Delete"};

handleDel(_,[MeasurerName]) ->
    case measurerExists(MeasurerName) of
        true ->
            deleteTable(MeasurerName),
            deleteRecord(measurers,MeasurerName),
            {html,"Delete" ++ MeasurerName};
        false ->
            {html,"wrong way"}
    end;

handleDel(_,_) ->
    {html,"wrong way"}.

%%% admin

authenticate(Table,Name,Password) ->
    case select(Table,Name) of
        [{Table,_,Pass}] ->
            string:equal(Pass,Password);
        _ -> false
    end.

addMeasurer(Name,Password) ->
    F = fun() ->
        application:start(mnesia),
        mnesia:create_table(Name,
            [{attributes, record_info(fields,measures)},
                {index,[#measures.timestamp]},
                {disc_copies, node()}]),

        Fun = fun() ->
            mnesia:write(#measurers{ name = Name, password = Password})
        end,
        mnesia:transaction(Fun),
        application:stop(mnesia)
    end,
    spawn(F).

addMeasurerFunction(Arg,Parent) ->
    case yaws_api:parse_post(Arg) of
        [{"name",Username},{"password",Password},{"measurer",MeasurerName},{"measurerPassword",MeasurerPassword}] ->
            case authenticate(admins,Username,Password) of
                false ->
                    %% TODO
                    Parent ! {self(),{html,"error"}};
                true ->
                    addMeasurer(MeasurerName,MeasurerPassword),
                    %% TODO return new url (measures/MeasurerName)
                    Parent ! {self(),{html,"success"}}
            end;
        _ -> Parent ! {self(),{ehtml,
            {pre, [],
                io_lib:format('~p', [yaws_api:parse_post(Arg)])}}}
    end.

%%% Measurers

measurerExists(Measurer) ->
    case select(measurers,Measurer) of
        [{measurers,_,_}] -> true;
        _ -> false
    end.

%%% mnesia

select_all(Table) ->
    application:start(mnesia),
    F = fun() -> mnesia:select(Table,[{'_',[],['$_']}]) end,
    mnesia:activity(transaction, F).

select(Table,Index) ->
    application:start(mnesia),
    F = fun() -> mnesia:match_object(Table,{Table,Index,'_'},read) end,
    mnesia:activity(transaction,F).

selectWithLimit(Table,Limit) ->
    application:start(mnesia),
    F = fun() -> mnesia:select(Table,[{'_',[],['$_']}],Limit,read) end,
    mnesia:activity(transaction,F).

deleteTable(Table) ->
    application:start(mnesia),
    F = fun() -> mnesia:delete_table(Table) end,
    mnesia:activity(transaction,F),
    application:stop(mnesia).

deleteRecord(Table,Record) ->
    application:start(mnesia),
    F = fun() -> mnesia:delete({Table,Record}) end,
    mnesia:activity(transaction,F),
    application:stop(mnesia).