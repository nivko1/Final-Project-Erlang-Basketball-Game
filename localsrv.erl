%% @author niv
%% @doc @todo Add description to 314962382.


-module(localsrv).

%% ====================================================================
%% API functions
%% ====================================================================
-behaviour(gen_server).
-include("defineConsts.hrl").
-include_lib("stdlib/include/qlc.hrl").


-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
-export([startgen/4, addPlayers/2, addBotPlayer/2, addBall/2, refreshInfo/3, addUser/2, checkServers/5, refreshLocation/3, switchArea/2, insertComponent/3, deleteAll/1, shutDown/1]).

-record(localRecord, {localName, tables, deleteEts, pid_ReceiveBlock, localID, monitorsNumber, allLocals}). %record data structure


startgen(Name, ID, MonitorsNumber, LocalsNames) -> 
%function that starting the gen_server
  FunToSpawn = fun() ->
    receiveBlock(Name, [])
               end,
  PID_ReceiveBlock = spawn(FunToSpawn),
  group_leader(whereis(user), self()), 
  % sets the group leader of seld pid to be leader and responssible on the I/O interface.
  %use  is a server that responds to all messages defined in the I/O messages
  gen_server:start_link({global, Name}, ?MODULE, [ID, Name, PID_ReceiveBlock, MonitorsNumber, LocalsNames], []). %linked and goes to init


init([ID, Name, PID_ReceiveBlock, MonitorsNumber, LocalsNames]) ->
  Self = self(), global:register_name(Name, Self),
  EtsToDelete = ets:new(deleteEts, [set]), %return table
  EtsTables = ets:new(tables, [set, public, named_table]), %return table
  PID_ReceiveBlock ! {localsrv, Self}, 
  %send message to local server RB
  busyWait(), 
  %call function for polling 
  State = #localRecord{localName = Name, tables = EtsTables, deleteEts = EtsToDelete, pid_ReceiveBlock = PID_ReceiveBlock,  %DECLARE state 
    localID = ID, monitorsNumber = MonitorsNumber, allLocals = LocalsNames},
  erlang:send_after(50, self(), refreshMonitor),
  {ok, State}.

checkServers(Name, ID, MonitorsNumber, LocalsNames, ComponentsInMonitor) -> %function that check the active computer and refresh them
  gen_server:cast({global, Name}, {checkServers, ID, MonitorsNumber, LocalsNames, ComponentsInMonitor}).

%% casting function for the gen_statem and handling them after

refreshInfo(Name, BallOrPlayerID, Arguments) ->
  gen_server:cast({global, Name}, {refreshInfo, BallOrPlayerID, Arguments}).

insertComponent(Name, ID, Arguments) ->
  gen_server:cast({global, Name}, {insertComponent, ID, Arguments}).

addUser(Name, {ID, Location, OldOrNew}) ->
  gen_server:cast({global, Name}, {addUser, ID, Location, OldOrNew}).

addBotPlayer(Name, {ID, Location, Dest}) ->
  gen_server:cast({global, Name}, {addBotPlayer, ID, Location, Dest}).

addPlayers(Name, List) ->
  gen_server:cast({global, Name}, {addPlayers, List}).

addBall(Name, {ID, Location, Destination, Exist}) ->
  gen_server:cast({global, Name}, {addBall, ID, Location, Destination, Exist}).

refreshLocation(Name, BallOrPlayerID, Location) ->
  gen_server:cast({global, Name}, {refreshLocation, BallOrPlayerID, Location}).

switchArea(Name, BallOrPlayerID) ->
  gen_server:cast({global, Name}, {switchArea, BallOrPlayerID}).

deleteAll(Name) ->
  gen_server:cast({global, Name}, {deleteAll}).

shutDown(Name) ->
  gen_server:cast({global, Name}, {shutDown}).

busyWait() -> 
  IsDefined = global:whereis_name(mainsrv), 
  %returtns the pid with the globally registerd undeer the name mainsrv, else undefined
  case IsDefined of
    undefined -> busyWait();
    _ -> ok
  end.

handle_call(_, _, Data) ->
  {reply, Data}.

handle_cast({addPlayers, List}, Data) ->
  FunForEach = fun({ID, {_ObjType, Location, {Destination}}}) ->
    NameFromState = Data#localRecord.localName,
    botplayers:startgen(ID, Location, Destination, NameFromState)
               end,
  lists:foreach(FunForEach, List),
  {noreply, Data};

handle_cast({addBotPlayer, ID, Location, Destination}, Data) ->
  NameFromState = Data#localRecord.localName,
  botplayers:startgen(ID, Location, Destination, NameFromState),
  {noreply, Data};

handle_cast({addBall, ID, Location, Destination, _}, Data) ->
  NameFromState = Data#localRecord.localName,
  basketball:startgen(ID, Location, Destination, NameFromState),
  {noreply, Data};

handle_cast({addUser, ID, Location, _}, Data) ->
  userplayer:startgen(ID, Location, Data#localRecord.localName),
  {noreply, Data};

handle_cast({insertComponent, BallOrPlayerID, Arguments}, Data) ->
  MonitorID = ets:lookup(Data#localRecord.tables, BallOrPlayerID),
  case MonitorID of
    [] ->
      ets:insert(Data#localRecord.tables, {BallOrPlayerID, Arguments});
    _ -> continue
  end,
  {noreply, Data};

handle_cast({refreshInfo, PlayerID, Arguments}, Data) ->
  GetPlayer = ets:lookup(Data#localRecord.tables, PlayerID),
  case GetPlayer of
    [] -> continue;
    [{_, {userplayer, _, _}}] ->
      {userplayer, Location, {MonitorName}} = Arguments,
      IsInside = isInsideCurrentScreen(Location, Data#localRecord.localID, Data#localRecord.monitorsNumber, userplayer),
      case IsInside of
        false ->
          switchAreaBot(Data, PlayerID);
        true ->
          ets:insert(Data#localRecord.tables, {PlayerID, {userplayer, Location, {MonitorName}}})
      end;
    [{_, {computerplayer, _, {_}}}] ->
      {ObjType, Location, {Destination}} = Arguments,
      IsInside = isInsideCurrentScreen(Location, Data#localRecord.localID, Data#localRecord.monitorsNumber, computerplayer),
      case IsInside of
        false ->
          switchAreaUser(Data, PlayerID);
        true ->
          ets:insert(Data#localRecord.tables, {PlayerID, {ObjType, Location, {Destination}}})
      end;
    [{_, {ball, _, {_}}}] ->
      {ObjType, Location, {Destination}} = Arguments,
      IsInside = isInsideCurrentScreen(Location, Data#localRecord.localID, Data#localRecord.monitorsNumber, ball),
      case IsInside of
        false ->
          switchAreaBall(Data, PlayerID);
        true ->
          ets:insert(Data#localRecord.tables, {PlayerID, {ObjType, Location, {Destination}}})
      end
  end,
  {noreply, Data};


handle_cast({checkServers, ID, MonitorsNumber, LocalsNames, ComponentsInMonitor}, Data) ->
  NewState = Data#localRecord{monitorsNumber = MonitorsNumber, localID = ID, allLocals = LocalsNames},
  AllPlayersL = ets:tab2list(Data#localRecord.tables),
  ets:delete_all_objects(Data#localRecord.tables),
  FunForEach1 = fun({PlayerID, {ObjType, _, _}}) ->
    case ObjType of
      userplayer -> userplayer:delete(PlayerID);
      computerplayer -> botplayers:delete(PlayerID);
      ball -> basketball:delete(PlayerID)
    end
                end,
  lists:foreach(FunForEach1, AllPlayersL),
  FunForEach2 = fun({_, {ObjType, Location, Args}, MyId}) ->
    case ObjType of
      userplayer ->
        userplayer:startgen(userplayer, Location, Data#localRecord.localName);
      computerplayer ->
        {Destination} = Args,
        botplayers:startgen(MyId, Location, Destination, Data#localRecord.localName);
      ball ->
        {Destination} = Args,
        basketball:startgen(MyId, Location, Destination, Data#localRecord.localName);
      _ -> continue
    end end,
  lists:foreach(FunForEach2, ComponentsInMonitor),
  {noreply, NewState};


handle_cast({refreshLocation, BallOrPlayerID, Location}, Data) ->
  userplayer:move(BallOrPlayerID, Location),
  {noreply, Data};


handle_cast({switchArea, BallOrPlayerID}, Data) ->
  ets:delete(Data#localRecord.tables, BallOrPlayerID),
  {noreply, Data};


handle_cast({deleteAll}, Data) ->
  etsDeleteAll(Data#localRecord.tables),
  {noreply, Data};


handle_cast({shutDown}, Data) ->
  {stop, shutdown, Data};


handle_cast(_Result, Data) ->
  {noreply, Data}.


handle_info(refreshMonitor, Data) ->
  EtsTables = ets:tab2list(Data#localRecord.tables),
  EtsToDelete = ets:tab2list(Data#localRecord.deleteEts),
  WxPID = global:whereis_name(mainsrv),
  master:updateDB(EtsTables, EtsToDelete, Data#localRecord.localName, WxPID),
  ets:delete_all_objects(Data#localRecord.deleteEts),
  erlang:send_after(50, self(), refreshMonitor),
  {noreply, Data};

handle_info(_, Data) ->
  {noreply, Data}.


terminate(_, Data) ->
  Data#localRecord.pid_ReceiveBlock ! kill,
  etsDeleteAll(Data#localRecord.tables).


etsDeleteAll(ToDeleteEts) ->
  ToDeleteList = ets:tab2list(ToDeleteEts),
  ets:delete_all_objects(ToDeleteEts),
  FunToDel = fun({ID, {ObjType, _, _}}) ->
    case ObjType of
      userplayer -> userplayer:delete(ID);
      computerplayer -> botplayers:delete(ID);
      ball -> basketball:delete(ID)
    end
             end,
  lists:foreach(FunToDel, ToDeleteList).


receiveBlock(MonitorName, ToAdd) ->
  receive
    {localsrv, MonitorPid} ->
      IsDefiend = global:whereis_name(mainsrv),
      master:linkProc(IsDefiend, MonitorName),
      erlang:monitor(process, MonitorPid),
      receiveBlock(MonitorName, ToAdd);
    kill ->
      stop;
    _ -> continue
  end.

switchAreaBot(Data, PlayerID) ->
  userplayer:switchArea(PlayerID, Data#localRecord.monitorsNumber, Data#localRecord.allLocals).


switchAreaUser(Data, PlayerID) ->
  botplayers:switchArea(PlayerID, Data#localRecord.monitorsNumber, Data#localRecord.allLocals).


switchAreaBall(Data, PlayerID) ->
  basketball:switchArea(PlayerID, Data#localRecord.monitorsNumber, Data#localRecord.allLocals).


isInsideCurrentScreen({X, _}, ID, NumOfMonitors, _) ->
  {LeftEdge, RightEdge} = checkWhichMonitor(ID, NumOfMonitors),
  ((X =< RightEdge) and (X >= LeftEdge)).


checkWhichMonitor(MonitorNum, MonitorsNumber) ->
  MonitorSize = (?X_Upper_Limit + 1) / MonitorsNumber,
  MonitorXEdgeMin = MonitorSize * (MonitorNum - 1),
  MonitorXEdgeMax = MonitorSize * MonitorNum - 1,
  case {MonitorXEdgeMin, MonitorXEdgeMax} of
    {0, ?X_Upper_Limit} ->
      Result = {-100, ?X_Upper_Limit + 100};
    {0, _} ->
      Result = {-100, MonitorXEdgeMax};
    {_, ?X_Upper_Limit} ->
      Result = {MonitorXEdgeMin, ?X_Upper_Limit + 100};
    {_, _} ->
      Result = {MonitorXEdgeMin, MonitorXEdgeMax}
  end, Result.

