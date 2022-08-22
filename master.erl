%% @author niv
%% @doc @todo Add description to 314962382.


-module(master).

%% ====================================================================
%% API functions
%% ====================================================================

-behaviour(wx_object). %for GUI
-include_lib("stdlib/include/qlc.hrl").
-include_lib("wx/include/wx.hrl").
-include("defineConsts.hrl").

-export([startgen/0, updateDB/4, linkProc/2, finishedGame/1, serverDown/1, init/1, handle_event/2, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(guiRecord, {wXPanel, wxFrame, wxBall, self, quarter}).


startgen() -> %function that starting the gen_server - wx_object , first compiling the other files
  compile:file(basketball), compile:file(userplayer),
  compile:file(botplayers), compile:file(localsrv),
  compile:file(helpfunctions), compile:file(etsTables),
  WX = wx:new(), wx_object:start_link({global, mainsrv}, ?MODULE, WX, []). %linking between wx objects processes

linkProc(ID, MonitorName) -> %casting for linking local computers
  gen_server:cast(ID, {linkProc, MonitorName}).

finishedGame(ID) ->
  gen_server:cast(ID, {finishedGame}).

serverDown(ID) ->
  gen_server:cast(ID, {serverDown}).

init(ID) -> %initialize server
  FunToBatch = fun() -> initializeGame(ID) end,
  wx:batch(FunToBatch).

initializeGame(ID) -> %the first init of the game with gui and devide res areas for the other computers 
  IsMainDefined = whereis(mainsrv),
  if IsMainDefined /= undefined ->
    unregister(mainsrv),
    rpc:multicall(?PROCESSES_AREA, global, unregister, [mainsrv]);
    true -> continue
  end,
  global:register_name(mainsrv, self()), helpfunctions:startingConnection(), etsTables:initEts(), %controlling areas
  FunToSpawn1 = fun() -> %send RPC concurrently from one client to multiple clinets for compiling
    rpc:multicall(?PROCESSES_AREA, compile, file, [userplayer]), rpc:multicall(?PROCESSES_AREA, compile, file, [botplayers]),
    rpc:multicall(?PROCESSES_AREA, compile, file, [localsrv]), rpc:multicall(?PROCESSES_AREA, compile, file, [basketball]),
    rpc:multicall(?PROCESSES_AREA, compile, file, [master]), rpc:multicall(?PROCESSES_AREA, compile, file, [etsTables]),
    rpc:multicall(?PROCESSES_AREA, compile, file, [helpfunctions]), rpc:call(?COMPUTER_NAME1, localsrv, startgen, [?LOCAL1, 1, 4, [?LOCAL1, ?LOCAL2, ?LOCAL3, ?LOCAL4]]),
    rpc:call(?COMPUTER_NAME2, localsrv, startgen, [?LOCAL2, 2, 4, [?LOCAL1, ?LOCAL2, ?LOCAL3, ?LOCAL4]]), rpc:call(?COMPUTER_NAME3, localsrv, startgen, [?LOCAL3, 3, 4, [?LOCAL1, ?LOCAL2, ?LOCAL3, ?LOCAL4]]),
    rpc:call(?COMPUTER_NAME4, localsrv, startgen, [?LOCAL4, 4, 4, [?LOCAL1, ?LOCAL2, ?LOCAL3, ?LOCAL4]]) end,
  FunToSpawn2 = fun() -> helpfunctions:localsControl() end,
  spawn(FunToSpawn1), spawn(FunToSpawn2),
  WXFrame = wxFrame:new(ID, -1, "Basket StreetBall Game", [{size, {1439, 819}}]),
  WXPanel = wxPanel:new(WXFrame, [{style, ?wxFULL_REPAINT_ON_RESIZE}]),
  wxFrame:createStatusBar(WXFrame), wxFrame:connect(WXPanel, motion),
  wxFrame:connect(WXFrame, command_menu_selected, []), wxFrame:connect(WXPanel, left_up),
  wxWindow:setBackgroundColour(WXFrame, {200, 200, 200}), wxFrame:show(WXFrame),
  WXBall = wxImage:new(?BALL_IMAGE), GuiRecord = #guiRecord{wXPanel = WXPanel, wxFrame = WXFrame, wxBall = WXBall, self = self(), quarter = 0},
  WXIMGBackground = wxImage:new(?STAGE_IMAGE),
  ConnectFrame = fun(#wx{event = #wxPaint{}}, _wxObj) ->
    WxBufferedPaint = wxBufferedPaintDC:new(WXPanel),
    ets:insert(stats, {temp, WxBufferedPaint}), WXBitmap = wxBitmap:new(WXIMGBackground),
    wxDC:drawBitmap(WxBufferedPaint, WXBitmap, {0, 0}),
    wxBitmap:destroy(WXBitmap), wxDC:setTextForeground(WxBufferedPaint, ?wxBLACK),
    helpfunctions:gameLoop(WxBufferedPaint),
    wxBufferedPaintDC:destroy(WxBufferedPaint) end,
  wxFrame:connect(WXPanel, paint, [{callback, ConnectFrame}]),
  timer:send_interval(15, self(), refreshWindow),
  {WXPanel, GuiRecord}.

handle_event(#wx{obj = _, event = #wxMouse{type = motion, x = XCoordinate, y = YCoordinate}}, State) -> %function the contolling the User player with the mouse
  case ets:lookup(players, userplayer) of
    [] -> continue;
    [{_, {_, {_, _}, {MonitorName}}, _}] ->
      DistanceLowerX = ?X_Lower_Limit + 15,
      DistanceUpperX = ?X_Upper_Limit - 15,
      DistanceLowerY = ?Y_Lower_Limit + 15,
      DistanceUpperY = ?Y_Upper_Limit - 15,
      case XCoordinate < DistanceLowerX of
        false -> X1Coordinate = XCoordinate;
        _ -> X1Coordinate = DistanceLowerX
      end,
      X2Coordinate = X1Coordinate,
      case X2Coordinate > DistanceUpperX of
        false -> X3Coordinate = X2Coordinate;
        _ -> X3Coordinate = DistanceUpperX
      end,
      case YCoordinate < DistanceLowerY of
        false -> Y1Coordinate = YCoordinate;
        _ -> Y1Coordinate = DistanceLowerY
      end,
      Y2Coordinate = Y1Coordinate,
      case Y2Coordinate > DistanceUpperY of
        false -> Y3Coordinate = Y2Coordinate;
        _ -> Y3Coordinate = DistanceUpperY
      end,
      {X4Coordinate, Y4Coordinate} = {X3Coordinate, Y3Coordinate},
      localsrv:refreshLocation(MonitorName, userplayer, {X4Coordinate, Y4Coordinate})
  end,
  {noreply, State};

handle_event(#wx{event = #wxMouse{type = left_up}}, Data) -> %handle events from the server
  [{_, Status}] = ets:lookup(generalState, status),
  case Status of
    startGame ->
      helpfunctions:startQuarter(1, true),
      ets:insert(generalState, {status, idle});
    newQuarter ->
      helpfunctions:newQuarter(),
      ets:insert(generalState, {status, idle});
    finishedGame ->
      stay;
    idle ->
      helpfunctions:shoot(Data#guiRecord.wxBall);
    _ -> continue
  end,
  {noreply, Data};

handle_event(#wx{}, State) ->
  {noreply, State}.

handle_call(_, _, State) ->
  {stop, normal, ok, State}.

handle_cast({updateDB, EntriesToChange, WhatToDelete, _}, State) ->
  [{_, Status}] = ets:lookup(generalState, status),
  case Status of
    idle ->
      FunInsertPlayers = fun() ->
        lists:foreach(
          fun({ID, Args}) -> [{_, Status}] = ets:lookup(generalState, status),
            if Status =:= idle ->
              ets:insert(players, {ID, Args, ID})
            end
          end, EntriesToChange) end,
      FunDelPlayers = fun() -> lists:foreach(fun({ID, _}) ->
        ets:delete(players, ID) end, WhatToDelete) end,
      spawn(FunDelPlayers), spawn(FunInsertPlayers);
    _ -> continue
  end,
  {noreply, State};

handle_cast({linkProc, MonitorName}, Data) ->
  case MonitorName of
    ?LOCAL1 ->
      addComputer(?LOCAL1);
    ?LOCAL2 ->
      addComputer(?LOCAL2);
    ?LOCAL3 ->
      addComputer(?LOCAL3);
    ?LOCAL4 ->
      addComputer(?LOCAL4)
  end,
  {noreply, Data};

handle_cast({serverDown}, State) ->
  {stop, shutdown, State};

handle_cast(finishedGame, State) ->
  ets:insert(generalState, {status, finishedGame}),
  helpfunctions:getMonitorsAndDeleteAll(),
  ets:delete_all_objects(players),
  {noreply, State};

handle_cast(_Cast, State) ->
  {noreply, State}.

updateDB(EntriesToChange, WhatToDelete, MonitorName, ID) ->
  gen_server:cast(ID, {updateDB, EntriesToChange, WhatToDelete, MonitorName}).

addComputer(MonitorName) ->
  case ets:lookup(computers, MonitorName) of
    [] ->
      ets:insert(computers, {MonitorName, {true}}),
      List = ets:tab2list(computers),
      case length(List) of
        4 -> ets:insert(generalState, {status, startGame});
        _ -> continue
      end;
    [{MonitorName, {false}}] ->
      helpfunctions:compReconnect(MonitorName),
      ets:insert(computers, {MonitorName, {true}})
  end.

handle_info(refreshWindow, Data) ->
  wxWindow:refresh(Data#guiRecord.wXPanel, [{eraseBackground, false}]),
  {noreply, Data};

handle_info(_, Data) ->
  {noreply, Data}.

code_change(_, _, Data) ->
  {stop, ignore, Data}.

terminate(_, Data) ->
  FunGetMonitors = fun({_, {A}}) -> A end,
  MList = ets:tab2list(computers),
  ListOfMonitors = lists:filter(FunGetMonitors, MList),
  lists:foreach(fun({Monitor, _}) ->
    localsrv:shutDown(Monitor) end, ListOfMonitors),
  PIDA = whereis(?LOCAL1),
  PIDB = whereis(?LOCAL2),
  PIDC = whereis(?LOCAL3),
  PIDD = whereis(?LOCAL4),
  if PIDA /= undefined ->
    PIDA ! kill;
    true -> continue
  end,
  if PIDB /= undefined ->
    PIDB ! kill;
    true -> continue
  end,
  if PIDC /= undefined ->
    PIDC ! kill;
    true -> continue
  end,
  if PIDD /= undefined ->
    PIDD ! kill;
    true -> continue
  end,
  wxPanel:destroy(Data#guiRecord.wXPanel),
  wx:destroy(), ok.
