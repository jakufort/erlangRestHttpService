%%%-------------------------------------------------------------------
%%% @author fortun
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 06. sty 2015 15:36
%%%-------------------------------------------------------------------
-module(dbNode).
-author("fortun").

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {}).
-record(measurers,{name,password}).
-record(measures,{timestamp,measure}).

%%%===================================================================
%%% API
%%%===================================================================

%%probably will have to spawn new process for every action (just because i can)

initialize() ->
  ok = mnesia:create_schema(node()),
  application:start(mnesia),
  mnesia:create_table(measurers,
    [{attributes, record_info(fields, measurers)},
      {index, [#measurers.name]},
      {disc_copies, node()}]),
  application:stop(mnesia).

addMeasureNode({Name,Password}) ->
  application:start(mnesia),
  mnesia:create_table(Name,
    [{attributes, record_info(fields,measures)},
      {index,[#measures.timestamp]},
      {disc_copies, node()}]),

  Fun = fun() ->
    mnesia:write(
      #measurers{ name = Name, password = Password}
    )
    end,
  mnesia:transaction(Fun),
  application:stop(mnesia).

addNewMeasure(Name,Measure, Time) ->
  %{M,S,MS} = now(),
  %M * 1000000 * 1000000 + S * 1000000 + MS,
  Fun = fun() ->
    mnesia:write(
      Name, #measures{ timestamp = Time, measure = Measure}, write
    )
    end,
  mnesia:transaction(Fun).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
  {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([]) ->
  {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
  {reply, Reply :: term(), NewState :: #state{}} |
  {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
  {stop, Reason :: term(), NewState :: #state{}}).

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).

handle_cast(_Request, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).

handle_info({Measurer,Password,MeasureTime,Measure},State) ->
  Fun = fun() -> case checkMeasurer(Measurer,Password) of
                   true -> addNewMeasure(Measurer,MeasureTime,Measure)
                 end
        end,
  spawn(Fun),
  {noreply,State}.

handle_info(_Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).

terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
  {ok, NewState :: #state{}} | {error, Reason :: term()}).

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This is used to determine if measurer credentials are all right
%%
%% @end
%%--------------------------------------------------------------------
checkMeasurer(Measurer,Password) ->
  case select(measurers,Measurer) of
    {_,Pass} ->
      Pass=Password;
    _ ->
      false
  end.
  %TODO

%%% mnesia

select(Table,Index) ->
  mnesia:start(),
  Fun = fun() ->
    mnesia:read({Table,Index})
  end,
  {atomic,[Row]} = mnesia:transaction(Fun),
  mnesia:stop(),
  Row.
