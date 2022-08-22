%% @author niv
%% @doc @todo Add description to helpfunctions.

-module(helpfunctions).
-include("defineConsts.hrl").
-include_lib("wx/include/wx.hrl").
-include_lib("stdlib/include/qlc.hrl").

-export([positionList/0, showBotPic/4, showUserPic/4, displayChar/3, initializeBallLocation1/0, initializeBallLocation/0]).
-export([gameLoop/1, startingConnection/0, localsControl/0, compReconnect/1, shoot/1, newQuarter/0, startQuarter/2, calcDis/2, movingPlayers/3, moveBall/3, pickArea/3]).

%this function responssible on the flow of the game when playing each quarter
mainGameHandler(WXPaint, Id, Type, {GivenX, GivenY}, Args) -> 
  [{_, BallX}] = ets:lookup(ball, ballX),
  [{_, BallY}] = ets:lookup(ball, ballY), %getting infromation from the ets tables
  [{_, WhoWithBall}] = ets:lookup(isOwner, ball),
  IsFinishedGame = isFinishedGame(BallX, BallY),
  BallImg = wxImage:new(?BALL_IMAGE),
  BallWidth = wxImage:getWidth(BallImg),
  [{_, BallPreviousOwner}] = ets:lookup(ball, previousOwner),
  if IsFinishedGame == 1 ->
    getMonitorsAndDeleteAll(),
    ets:delete_all_objects(players),
    ets:insert(generalState, {status, finishedGame});
    true ->
      if (Id < 3) or (Id == userplayer) ->
        case Type of
          computerplayer ->
            PicToDraw = mainBot(Id, GivenX, GivenY, BallX, BallY, BallPreviousOwner, BallWidth, GivenX, Args, WhoWithBall, WXPaint, BallImg);
          userplayer ->
            PicToDraw = mainUser(GivenX, GivenY, BallX, BallY, BallPreviousOwner, BallWidth, WXPaint, BallImg, Id);
          ball ->
            PicToDraw = mainBall(Id, GivenX,BallWidth)
        end,
        Width = wxImage:getWidth(PicToDraw),
        CalcX = GivenX - Width / 2,
        Height = wxImage:getHeight(PicToDraw),
        CalcY = GivenY - Height / 2,
        paintImage(WXPaint, PicToDraw, {CalcX, CalcY}),
        wxImage:destroy(PicToDraw);
        true ->
          [{_, ShootDirection}] = ets:lookup(isOwner, shootDirection),
          [{_, Owned}] = ets:lookup(isOwner, owned),
          if (WhoWithBall == 0) ->
            if
              ((ShootDirection == 0) and (Owned == 0)) ->
                updateBallEts(WXPaint, BallImg, BallX, BallY);
              (ShootDirection == 1) ->
                {NewPosX, NewPosY} = moveBall({BallX, BallY}, randomNetDest(left), 40),
                updateBallEts(WXPaint, BallImg, NewPosX, NewPosY);
              (ShootDirection == 2) ->
                {NewPosX, NewPosY} = moveBall({BallX, BallY}, randomNetDest(right), 40),
                updateBallEts(WXPaint, BallImg, NewPosX, NewPosY);
              true -> continue
            end;
            true -> continue
          end
      end
  end.

gameLoop(WXPaint) -> %this is the main game loop function and handling in what state we are along the game
  [{_, Status}] = ets:lookup(generalState, status),
  Font = wxFont:new(28, ?wxFONTFAMILY_SCRIPT, ?wxFONTSTYLE_NORMAL, ?wxBOLD), %picking fint and gui parameters
  wxDC:setFont(WXPaint, Font),
  case Status of %state case for the game
    idle ->  %beggining of the game, upload openning image 
      WXIMGBackground = wxImage:new(?COURT_IMAGE),
      WXBitmap = wxBitmap:new(WXIMGBackground),
      wxDC:drawBitmap(WXPaint, WXBitmap, {0, 0}),
      wxBitmap:destroy(WXBitmap),
      List = ets:tab2list(players),
      displayCharacters(WXPaint, List),
      ShouldPrint = false;
    startGame -> %disppaly the starting menue
      wxDC:drawLabel(WXPaint, "Welcome to StreetBall-NBA Game!", {?PRINT_X_START, ?PRINT_W_H - 7*?TAB, ?PRINT_W_H, ?PRINT_W_H}),
      wxDC:drawLabel(WXPaint, "         Click to start", {?PRINT_X_START, ?PRINT_W_H - 5*?TAB, ?PRINT_W_H, ?PRINT_W_H}),
      wxDC:drawLabel(WXPaint, "You are playing with the mouse:", {?PRINT_X_START, ?PRINT_W_H + ?TAB, ?PRINT_W_H, ?PRINT_W_H}),
      wxDC:drawLabel(WXPaint, "Use the Mouse to move around the court and to get the basketball", {?PRINT_X_START - 9*?TAB, ?PRINT_W_H + 3*?TAB, ?PRINT_W_H, ?PRINT_W_H}),
      wxDC:drawLabel(WXPaint, "   Click to throw the ball into the basket", {?PRINT_X_START, ?PRINT_W_H + 6*?TAB, ?PRINT_W_H, ?PRINT_W_H}),
      ShouldPrint = false;
    newQuarter -> %dispalyng the after quarter stats and uploading the matched images gui
      WXIMGStage = wxImage:new(?STAGE_IMAGE),
      WXBitmap = wxBitmap:new(WXIMGStage),
      wxDC:drawBitmap(WXPaint, WXBitmap, {0, 0}),
      wxBitmap:destroy(WXBitmap),
      [{_, Quarter}] = ets:lookup(generalState, quarter),
      wxDC:drawLabel(WXPaint, "Quarter " ++ integer_to_list(Quarter) ++ " --- Click to continue ", {?PRINT_X_START - 4 * ?TAB, 50, ?PRINT_W_H, ?PRINT_W_H}),
      ShouldPrint = true;
    finishedGame -> % end game stats and image
      LastScreenImage = wxImage:new(?FINISHED_IMAGE),
      WXBitmap = wxBitmap:new(LastScreenImage),
      wxDC:drawBitmap(WXPaint, WXBitmap, {0, 0}),
      wxBitmap:destroy(WXBitmap),
      ShouldPrint = true;
    _ -> ShouldPrint = false
  end,
  displayChar(ShouldPrint, WXPaint, Status).

%this function is responssible on the mobing basketall aound the pitch while playing, update info and position and satates
mainBall(Id, GivenX,BallWidth) ->
  [{_, PreviousX}] = ets:lookup(playerDirection, Id),
  ets:insert(playerDirection, {Id, GivenX}),
  NewDir = GivenX - PreviousX,
  {_, DirPic} = showBotPic(Id, NewDir, BallWidth, GivenX),
  DrawImg = DirPic, DrawImg.
  
%this function is responssible on the moving the User aound the pitch while playing, update info and position and satates
mainUser(GivenX, GivenY, BallX, BallY, BallpreviousOwner, BallWidth, WXPaint, BallImg, Id) ->
  IsNear = locateBall(2, GivenX, GivenY, BallX, BallY),
  controlledPlayerStatsUpdate(IsNear, BallpreviousOwner),
  [{_, ContPlayerX}] = ets:lookup(userplayerX, xCoordinate),
  ets:insert(userplayerX, {xCoordinate, GivenX}),
  ContPlayerNewDir = GivenX - ContPlayerX,
  [{_, LastPos}] = ets:lookup(userplayerDir, direction),
  if ContPlayerNewDir == 0 -> ets:insert(userplayerDir, {direction, LastPos});
    true -> ets:insert(userplayerDir, {direction, ContPlayerNewDir})
  end,
  {XLocation, DrawImg} = showUserPic(ContPlayerNewDir, BallWidth, GivenX, LastPos),
  case IsNear of
    near ->
      insertBallsOwnerAndPosition(Id, GivenY, WXPaint, XLocation, BallImg, 1, XLocation, GivenY),
      ets:insert(isOwner, {ball, 1});
    _ ->
      ets:insert(isOwner, {ball, 0}),
      ets:insert(ball, {previousOwner, BallpreviousOwner})
  end, DrawImg.
  
%this function is responssible on the mobing bots aound the pitch while playing, update info and position and satates
mainBot(Id, GivenX, GivenY, BallX, BallY, BallpreviousOwner, BallWidth, GivenX, _, WhoWithBall, WXPaint, BallImg) -> 
  IsTeam1Near = locateBall(1, GivenX, GivenY, BallX, BallY),
  IsTeam2Near = locateBall(2, GivenX, GivenY, BallX, BallY),
  [{_, PreviousX}] = ets:lookup(playerDirection, Id),
  ets:insert(playerDirection, {Id, GivenX}),
  NewDir = GivenX - PreviousX,
  {X_img, DirPic} = showBotPic(Id, NewDir, BallWidth, GivenX),
  XLocation = X_img,
  if (WhoWithBall == 0) ->
    if
      ((IsTeam2Near == near) and (Id >= 2.0) and (Id < 3)) ->
        insertBallsOwnerAndPosition(Id, GivenY, WXPaint, XLocation, BallImg, 0, GivenX , GivenY),
        ets:insert(isOwner, {owned, 1}),
        if ((NewDir < 0) or ((Id == 2.0) and (NewDir == 0)) ) ->
          ets:insert(isOwner, {shootkDirection, 1});
          true -> continue
        end;
      ((IsTeam1Near == near) and (Id >= 1.0) and (Id < 2)) ->
        insertBallsOwnerAndPosition(Id, GivenY, WXPaint, XLocation, BallImg, 0, GivenX , GivenY), 
        ets:insert(isOwner, {owned, 1}),
        if ((NewDir > 0) or ((Id == 1.0) and (NewDir == 0))) ->
          ets:insert(isOwner, {shootDirection, 2});
          true -> continue
        end;
      true ->
        ets:insert(isOwner, {owned, 0}),
        ets:insert(ball, {previousOwner, BallpreviousOwner})
    end;
    true -> continue
  end,
  DrawImg = DirPic,
  DrawImg.

startingConnection() -> %sending first connecting ping using the inet package
  net_adm:ping(?COMPUTER_NAME1),
  net_adm:ping(?COMPUTER_NAME2),
  net_adm:ping(?COMPUTER_NAME3),
  net_adm:ping(?COMPUTER_NAME4).

localsControl() -> %starting processes for the loclas computers for evaluation
  MonitorA = spawn(fun() -> erlang:monitor_node(?COMPUTER_NAME1, true), receiveBlock(?COMPUTER_NAME1) end),
  MonitorB = spawn(fun() -> erlang:monitor_node(?COMPUTER_NAME2, true), receiveBlock(?COMPUTER_NAME2) end),
  MonitorC = spawn(fun() -> erlang:monitor_node(?COMPUTER_NAME3, true), receiveBlock(?COMPUTER_NAME3) end),
  MonitorD = spawn(fun() -> erlang:monitor_node(?COMPUTER_NAME4, true), receiveBlock(?COMPUTER_NAME4) end),
  register(monitorA, MonitorA),
  register(monitorB, MonitorB),
  register(monitorC, MonitorC),
  register(monitorD, MonitorD).

receiveBlock(Monitor) ->
  receive
    {nodedown, Monitor} ->
      case Monitor of
        ?COMPUTER_NAME1 ->
          MonitorName = ?LOCAL1;
        ?COMPUTER_NAME2 ->
          MonitorName = ?LOCAL2;
        ?COMPUTER_NAME3 ->
          MonitorName = ?LOCAL3;
        ?COMPUTER_NAME4 ->
          MonitorName = ?LOCAL4
      end,
      io:fwrite("~nLost Connection With local computer ~p~n", [MonitorName]),
      lostConnectionWithMonitor(MonitorName),
      connectToMonitor(Monitor, MonitorName);
    kill -> kill;
    _ -> receiveBlock(Monitor)
  end.

locateBall(WhichTeam, PlayerX, PlayerY, BallX, BallY) ->
  HowFarIsBall = calcDis({PlayerX, PlayerY}, {BallX, BallY}),
  if ((WhichTeam == 2) and (HowFarIsBall < 30)) or ((WhichTeam == 1) and (HowFarIsBall < 26)) ->
    IsNear = near;
    true -> IsNear = far
  end, IsNear.

insertBallsOwnerAndPosition(ID, _, Paint, _, Pic, _, NewX, NewY) ->
  [{_, PrevStatsUpdate}] = ets:lookup(stats, prevStatupdate),
  if ((ID /= userplayer) and (PrevStatsUpdate /= ID)) ->
    ets:insert(isOwner, {shootDirection, 0}),
    ets:insert(ball, {flag, false}),
    ets:insert(stats, {prevStatupdate, ID}),
    updateStatistics(ID, 1);
    true -> skip
  end,
  updateBallEts(Paint, Pic, NewX, NewY),
  ets:insert(ball, {previousOwner, ID}).

displayCharacters(_, []) -> continue;
displayCharacters(WXPaint, [{ID, {Type, Location, Args}, _} | T]) -> mainGameHandler(WXPaint, ID, Type, Location, Args), displayCharacters(WXPaint, T).

updateStatistics(ID, Offset) ->
  [{_, Amount}] = ets:lookup(stats, ID),
  ets:insert(stats, {ID, Amount + Offset}).


controlledPlayerStatsUpdate(IsNear, BallPreviousOwner) ->
  if IsNear == near ->
    ets:insert(ball, {flag, false}),
    if BallPreviousOwner == userplayer -> continue;
      true ->
        ets:insert(isOwner, {shootkDirection, 0}),
        updateStatistics(controlledPlayer, 1)
    end;
    true -> continue
  end.


paintImage(WXPaint, Image, {X, Y}) ->
  {RoundedX, RoundedY} = {round(X), round(Y)},
  Bitmap = wxBitmap:new(Image),
  wxDC:drawBitmap(WXPaint, Bitmap, {RoundedX, RoundedY}),
  wxBitmap:destroy(Bitmap).

updateBallEts(WXPaint, Pic, NewX, NewY) ->
  paintImage(WXPaint, Pic, {NewX, NewY}),
  wxImage:destroy(Pic),
  ets:insert(ball, {ballX, NewX}),
  ets:insert(ball, {ballY, NewY}).

restartMonitors() ->
  localsrv:shutDown(?COMPUTER_NAME1),
  localsrv:shutDown(?COMPUTER_NAME2),
  localsrv:shutDown(?COMPUTER_NAME3),
  localsrv:shutDown(?COMPUTER_NAME4),
  whereis(?LOCAL1) ! kill,
  whereis(?LOCAL2) ! kill,
  whereis(?LOCAL3) ! kill,
  whereis(?LOCAL4) ! kill,
  startingConnection(),
  FunToSpawn1 = fun() ->
    rpc:multicall(?PROCESSES_AREA, compile, file, [userplayer]),
    rpc:multicall(?PROCESSES_AREA, compile, file, [botplayers]),
    rpc:multicall(?PROCESSES_AREA, compile, file, [localsrv]),
    rpc:multicall(?PROCESSES_AREA, compile, file, [master]),
    rpc:multicall(?PROCESSES_AREA, compile, file, [helpfunctios]),
    rpc:multicall(?PROCESSES_AREA, compile, file, [basketball]),
    rpc:multicall(?PROCESSES_AREA, compile, file, [etsTables]),
    rpc:call(?COMPUTER_NAME1, localsrv, startgen, [?LOCAL1, 1, 4, [?LOCAL1, ?LOCAL2, ?LOCAL3, ?LOCAL4]]),
    rpc:call(?COMPUTER_NAME2, localsrv, startgen, [?LOCAL2, 2, 4, [?LOCAL1, ?LOCAL2, ?LOCAL3, ?LOCAL4]]),
    rpc:call(?COMPUTER_NAME3, localsrv, startgen, [?LOCAL3, 3, 4, [?LOCAL1, ?LOCAL2, ?LOCAL3, ?LOCAL4]]),
    rpc:call(?COMPUTER_NAME4, localsrv, startgen, [?LOCAL4, 4, 4, [?LOCAL1, ?LOCAL2, ?LOCAL3, ?LOCAL4]]) end,
  FunToSpawn2 = fun() -> localsControl() end,
  spawn(FunToSpawn1),
  spawn(FunToSpawn2).

startQuarter(QuarterNum, ShouldInsertControlledPlayer) ->
  if (QuarterNum /= 1) ->
    restartMonitors(),initializeBallEts1();
    true -> initializeBallEts(),cont
  end,
  ComponentsLayoutList = positionList(),
  [{_, Quarter}] = ets:lookup(generalState, quarter),
  ets:insert(generalState, {quarter, Quarter + 1}),
  NextMonitor = pickNextMonitor(720),
  case ShouldInsertControlledPlayer of
    false ->
      [{ID, {_, Location, {_}}}] = ets:lookup(generalState, userplayer),
      localsrv:addUser(NextMonitor, {ID, Location, old});
    _ -> localsrv:addUser(NextMonitor, {userplayer, {720, 600}, new})
  end,
  FunToMap = fun({ID, Loc, Dest}) -> {ID, {computerplayer, Loc, {Dest}}} end,
  AllPlayers = lists:map(FunToMap, ComponentsLayoutList),
  MonitorAPlayers = lists:filter(fun({_, {_, {X, _}, _}}) -> (pickNextMonitor(X) == ?LOCAL1) end, AllPlayers),
  MonitorBPlayers = lists:filter(fun({_, {_, {X, _}, _}}) -> (pickNextMonitor(X) == ?LOCAL2) end, AllPlayers),
  MonitorCPlayers = lists:filter(fun({_, {_, {X, _}, _}}) -> (pickNextMonitor(X) == ?LOCAL3) end, AllPlayers),
  MonitorDPlayers = lists:filter(fun({_, {_, {X, _}, _}}) -> (pickNextMonitor(X) == ?LOCAL4) end, AllPlayers),
  localsrv:addBall(pickNextMonitor(600), {ball, ?BALL_INITIAL_COORDINATES, ?BALL_INITIAL_COORDINATES, new}),
  localsrv:addPlayers(?LOCAL1, MonitorAPlayers),
  localsrv:addPlayers(?LOCAL2, MonitorBPlayers),
  localsrv:addPlayers(?LOCAL3, MonitorCPlayers),
  localsrv:addPlayers(?LOCAL4, MonitorDPlayers).

initializeBallEts1() ->
  initializeBallLocation1(),
  ets:insert(isOwner, {shootDirection, 0}),
  ets:insert(isOwner, {ball, 0}),
  ets:insert(isOwner, {owned, 0}),
  ets:insert(ball, {destX, 0}),
  ets:insert(ball, {destY, 0}),
  ets:insert(ball, {flag, false}).
  
initializeBallEts() ->
  initializeBallLocation(),
  ets:insert(isOwner, {shootDirection, 0}),
  ets:insert(isOwner, {ball, 0}),
  ets:insert(isOwner, {owned, 0}),
  ets:insert(ball, {destX, 0}),
  ets:insert(ball, {destY, 0}),
  ets:insert(ball, {flag, false}).

newQuarter() ->
  initializeBallEts1(),
  ComponentsLayoutList = positionList(),
  [{_, Quarter}] = ets:lookup(generalState, quarter),
  ets:insert(generalState, {quarter, Quarter + 1}),
  FunToMap = fun({ID, Location, Dest}) ->
    {ID, {computerplayer, Location, {Dest}}} end,
  AllPlayers = lists:map(FunToMap, ComponentsLayoutList),
  MonitorAPlayers = lists:filter(fun({_, {_, {X, _}, _}}) -> (pickNextMonitor(X) == ?LOCAL1) end, AllPlayers),
  MonitorBPlayers = lists:filter(fun({_, {_, {X, _}, _}}) -> (pickNextMonitor(X) == ?LOCAL2) end, AllPlayers),
  MonitorCPlayers = lists:filter(fun({_, {_, {X, _}, _}}) -> (pickNextMonitor(X) == ?LOCAL3) end, AllPlayers),
  MonitorDPlayers = lists:filter(fun({_, {_, {X, _}, _}}) -> (pickNextMonitor(X) == ?LOCAL4) end, AllPlayers),
  FunRefreshPlayerInMonitorA = fun({ID, {computerplayer, Location, {Dest}}}) ->
    localsrv:refreshInfo(?LOCAL1, ID, {computerplayer, Location, {Dest}}) end,
  FunRefreshPlayerInMonitorB = fun({ID, {computerplayer, Location, {Dest}}}) ->
    localsrv:refreshInfo(?LOCAL2, ID, {computerplayer, Location, {Dest}}) end,
  FunRefreshPlayerInMonitorC = fun({ID, {computerplayer, Location, {Dest}}}) ->
    localsrv:refreshInfo(?LOCAL3, ID, {computerplayer, Location, {Dest}}) end,
  FunRefreshPlayerInMonitorD = fun({ID, {computerplayer, Location, {Dest}}}) ->
    localsrv:refreshInfo(?LOCAL4, ID, {computerplayer, Location, {Dest}}) end,
  localsrv:refreshInfo(pickNextMonitor(600), ball, {ball, ?BALL_INITIAL_COORDINATES, {?BALL_INITIAL_COORDINATES}}),
  lists:map(FunRefreshPlayerInMonitorA, MonitorAPlayers),
  lists:map(FunRefreshPlayerInMonitorB, MonitorBPlayers),
  lists:map(FunRefreshPlayerInMonitorC, MonitorCPlayers),
  lists:map(FunRefreshPlayerInMonitorD, MonitorDPlayers).

pickNextMonitor(X) ->
  EtsToList = ets:tab2list(computers),
  FunForNumber = fun({_, {Act}}) -> Act end,
  FunForNames = fun({Monitor, _}) -> Monitor end,
  MonitorsList = lists:filter(FunForNumber, EtsToList),
  MonitorsNumber = length(MonitorsList),
  MonitorsNames = lists:map(FunForNames, MonitorsList),
  Ans = pickArea(X, MonitorsNumber, MonitorsNames),
  Ans.

lostConnectionWithMonitor(MonitorName) ->
  ets:insert(computers, {MonitorName, {false}}),
  FunToFilter = fun({_, {Active}}) -> Active end,
  EtsToList = ets:tab2list(computers),
  FilteredList = lists:filter(FunToFilter, EtsToList),
  case FilteredList of
    [] -> master:serverDown(global:whereis_name(mainsrv));
    _ -> ok
  end,
  refreshMonitorEts().

compReconnect(Name) ->
  ets:insert(computers, {Name, {true}}),
  refreshMonitorEts().

refreshMonitorEts() ->
  PlayersETSList = ets:tab2list(players),
  FunToFilter = fun({_, {FF}}) -> FF end,
  FunToMap = fun({DD, _}) -> DD end,
  EtsToList = ets:tab2list(computers),
  FilteredList = lists:filter(FunToFilter, EtsToList),
  ets:delete_all_objects(players),
  NumOfServers = length(FilteredList),
  NameOfServers = lists:map(FunToMap, FilteredList),
  updateScreenSizes(1, NumOfServers, NameOfServers, NameOfServers, PlayersETSList).

getMonitorsAndDeleteAll() ->
  Fun = fun({_, {Active}}) -> Active end,
  MList = ets:tab2list(computers),
  Fun2 = fun({A, _}) ->
    localsrv:deleteAll(A)
         end,
  MonitorsList = lists:filter(Fun, MList),
  lists:foreach(Fun2, MonitorsList).

connectToMonitor(MonitorLongName, MonitorName) ->
  receive
    kill -> kill
  after 9999 ->
    case net_adm:ping(MonitorLongName) of
      pong ->
        io:fwrite("~nReconnecting with local computer ~p~n", [MonitorName]),
        case rpc:call(MonitorLongName, compile, file, [localsrv]) of
          {ok, _} ->
            compileAllFiles(MonitorLongName),
            ConnectedMonitorsList = lists:sort(namesConnectedMonitors() ++ [MonitorName]),
            NumConnectedMonitors = countConnectedMonitors() + 1,
            rpc:call(MonitorLongName, localsrv, startgen, [MonitorName, monitorLongNameToNum(MonitorLongName), NumConnectedMonitors,
              ConnectedMonitorsList]),
            erlang:monitor_node(MonitorLongName, true),
            receiveBlock(MonitorLongName);
          _ ->
            connectToMonitor(MonitorLongName, MonitorName)
        end;
      pang ->
        connectToMonitor(MonitorLongName, MonitorName)
    end
  end.

monitorLongNameToNum(MonitorLongName) ->
  case (MonitorLongName) of
    ?COMPUTER_NAME4 -> MonitorId = 4;
    ?COMPUTER_NAME3 -> MonitorId = 3;
    ?COMPUTER_NAME2 -> MonitorId = 2;
    ?COMPUTER_NAME1 -> MonitorId = 1;
    _ -> MonitorId = 0
  end,
  MonitorId.

countConnectedMonitors() ->
  AIsAlive = isMonitorAlive(?LOCAL1),
  BIsAlive = isMonitorAlive(?LOCAL2),
  CIsAlive = isMonitorAlive(?LOCAL3),
  DIsAlive = isMonitorAlive(?LOCAL4),
  AIsAlive + BIsAlive + CIsAlive + DIsAlive.

namesConnectedMonitors() ->
  AIsAlive = isMonitorAlive(?LOCAL1),
  BIsAlive = isMonitorAlive(?LOCAL2),
  CIsAlive = isMonitorAlive(?LOCAL3),
  DIsAlive = isMonitorAlive(?LOCAL4),
  if (AIsAlive == 1) ->
    OutA = [?LOCAL1];
    true ->
      OutA = []
  end,
  if (BIsAlive == 1) ->
    OutB = [?LOCAL2];
    true ->
      OutB = []
  end,
  if (CIsAlive == 1) ->
    OutC = [?LOCAL3];
    true ->
      OutC = []
  end,
  if (DIsAlive == 1) ->
    OutD = [?LOCAL4];
    true ->
      OutD = []
  end,
  Output = lists:append([OutA, OutB, OutC, OutD]),
  Output.

isMonitorAlive(MonitorName) ->
  [{_, {IsALive}}] = ets:lookup(computers, MonitorName),
  if (IsALive == true) ->
    Res = 1;
    true ->
      Res = 0
  end,
  Res.

compileAllFiles(MonitorID) -> 
  rpc:call(MonitorID, compile, file, [userplayer]),
  rpc:call(MonitorID, compile, file, [basketball]),
  rpc:call(MonitorID, compile, file, [botplayers]),
  rpc:call(MonitorID, compile, file, [master]),
  rpc:call(MonitorID, compile, file, [helpfunctions]),
  rpc:call(MonitorID, compile, file, [etsTables]),
  rpc:call(MonitorID, compile, file, [localsrv]).

updateScreenSizes(N, N, [H], MonitorNames, AllPlayersAndBall) ->
  FunToFilter = fun({_, {_, {X, _}, _}, _}) -> (pickNextMonitor(X) == H) end,
  PlayersBelongToMonitor = lists:filter(FunToFilter, AllPlayersAndBall),
  localsrv:checkServers(H, N, N, MonitorNames, PlayersBelongToMonitor);

updateScreenSizes(M, N, [H | T], MonitorNames, AllPlayersAndBall) ->
  FunToFilter = fun({_, {_, {X, _}, _}, _}) -> (pickNextMonitor(X) == H) end,
  PlayersBelongToMonitor = lists:filter(FunToFilter, AllPlayersAndBall),
  localsrv:checkServers(H, M, N, MonitorNames, PlayersBelongToMonitor),
  updateScreenSizes(M + 1, N, T, MonitorNames, AllPlayersAndBall).

isFinishedGame(LocationX, LocationY) ->
  if (LocationX < 170) and (LocationX > 100) and (LocationY < ?BASKET_UP_LIM) and (LocationY > ?BASKET_LOW_LIM) ->
    IsFinished = goalUpdate(teamTwoPoints),
    initializeBallLocation(),ets:insert(ball, {lastTeamScored, 2}),
    IsFinished;
    (LocationX < 1339) and (LocationX > 1249) and (LocationY < ?BASKET_UP_LIM) and (LocationY > ?BASKET_LOW_LIM) ->
      IsFinished = goalUpdate(teamOnePoints),
      initializeBallLocation(),ets:insert(ball, {lastTeamScored, 1}),
      IsFinished;
    true -> 0
  end.

goalUpdate(TeamNumAtom) ->
  [{_, CurrentPoints}] = ets:lookup(stats, TeamNumAtom),
  NewRes = CurrentPoints + 2,
  ets:insert(stats, {TeamNumAtom, NewRes}),
  [{_, QuarterNum}] = ets:lookup(generalState,quarter),
  if (QuarterNum >= 4) -> 1;
    true ->
      ets:insert(generalState, {status, newQuarter}), 0
  end.

shoot(BallImg) ->
  [{_, IsBallAtPlayer}] = ets:lookup(isOwner, ball),
  if (IsBallAtPlayer == 1) ->
    [{_, WXPaint}] = ets:lookup(stats, temp),
    [{_, BallX}] = ets:lookup(ball, ballX),
    [{_, BallY}] = ets:lookup(ball, ballY),
    ets:insert(isOwner, {ball, 0}),
    Height = wxImage:getHeight(BallImg) / 22,
    paintImage(WXPaint, BallImg, {BallX - 90, BallY - Height}),
    ets:insert(ball, {ballX, BallX - 90}),
    ets:insert(ball, {ballY, BallY - Height}),
    ets:insert(isOwner, {shootDirection, 1});
    true -> continue
  end.

randomNetDest(WhichNet) ->
  [{_, DestBallX}] = ets:lookup(ball, destX),
  [{_, DestBallY}] = ets:lookup(ball, destY),
  [{_, Flag}] = ets:lookup(ball, flag),
  if (Flag == false) ->
    Random = rand:uniform(200),
    NewY = 500 - Random,
    if (WhichNet == left) ->
      Res = {45, NewY},
      ets:insert(ball, {destX, 45}),
      ets:insert(ball, {flag, true}),
      ets:insert(ball, {destY, NewY});
      (WhichNet == right) ->
        Res = {1390, NewY},
        ets:insert(ball, {destX, 1390}),
        ets:insert(ball, {flag, true}),
        ets:insert(ball, {destY, NewY})
    end;
    true -> Res = {DestBallX, DestBallY}
  end,
  Res.

showBotPic(ID, Orientation, BallWidth, X) ->
  if ID > 2.0 -> Pic = wxImage:new(?TEAMRED),
    if
      %(ID =:= 2.0) ->
      %  NewX = X - BallWidth / 135;
      (Orientation > 0) ->
        NewX = X + 10;
      true ->
        NewX = X - ?BALL_WIDTH_MULTIPLY_FACTOR * BallWidth
    end;
    true -> Pic = wxImage:new(?TEAMYELLOW),
      if
        %(ID =:= 1.0) ->
        % NewX = X + 10;
        (Orientation > 0) ->
          NewX = X + 10;
        true ->
          NewX = X - ?BALL_WIDTH_MULTIPLY_FACTOR * BallWidth
      end
  end,
  {NewX, Pic}.

showUserPic(Orientation, _, X, PreviousLocation) ->
  Pic = wxImage:new(?TEAMRED),
  if
    Orientation > 0 ->
      NewX = X + 10;
    Orientation == 0 ->
      if
        PreviousLocation > 0 ->
          NewX = X + 10;
        true ->
          NewX = X - 20
      end;
    Orientation < 0 ->
      NewX = X - 20
  end,
  {NewX, Pic}.

positionList() ->
  List = [
    {1.1, {100, 600}, {100, 600}}, {1.2, {200, 600}, {200, 600}},
    {1.3, {300, 550}, {300, 550}}, {1.4, {400, 450}, {400, 450}},
    {1.5, {500, 600}, {500, 600}}, {2.1, {1040, 600}, {1050, 600}},
    {2.2, {1100, 600}, {450, 400}}, {2.3, {700, 500}, {460, 470}},
    {2.4, {900, 420}, {480, 320}}], List.

displayChar(ShouldPrint, WXPaint, Status) ->
  case (ShouldPrint) of
    true ->
      [{_, Player1_1}] = ets:lookup(stats, 1.1), [{_, Player1_2}] = ets:lookup(stats, 1.2), [{_, Player1_3}] = ets:lookup(stats, 1.3),
      [{_, Player1_4}] = ets:lookup(stats, 1.4), [{_, Player1_5}] = ets:lookup(stats, 1.5),
      [{_, Player2_1}] = ets:lookup(stats, 2.1), [{_, Player2_2}] = ets:lookup(stats, 2.2),
      [{_, Player2_3}] = ets:lookup(stats, 2.3), [{_, Player2_4}] = ets:lookup(stats, 2.4),
      [{_, ControlledPlayer}] = ets:lookup(stats, controlledPlayer),
      if Status == finishedGame ->
        Winner = winnerTeam(),
        if (Winner =:= 1) ->
          wxDC:drawLabel(WXPaint, "YellowTeam Wins", {?PRINT_X_START - 5*?DOUBLE_TAB, ?PRINT_Y_START + ?TAB, ?PRINT_W_H, ?PRINT_W_H}),
          [{_, TeamOnePoints}] = ets:lookup(stats, teamOnePoints),
          [{_, TeamTwoPoints}] = ets:lookup(stats, teamTwoPoints);
          true -> wxDC:drawLabel(WXPaint, "RedTeam Wins", {?PRINT_X_START - 5*?DOUBLE_TAB, ?PRINT_Y_START + ?TAB, ?PRINT_W_H, ?PRINT_W_H}),
            [{_, TeamOnePoints}] = ets:lookup(stats, teamOnePoints),
            [{_, TeamTwoPoints}] = ets:lookup(stats, teamTwoPoints)
         end;
        true ->
          [{_, TeamOnePoints}] = ets:lookup(stats, teamOnePoints),
          [{_, TeamTwoPoints}] = ets:lookup(stats, teamTwoPoints)
      end,
      wxDC:drawLabel(WXPaint, "Game's Processes Statistics:", {?PRINT_X_START - 5*?DOUBLE_TAB, 100, ?PRINT_W_H, ?PRINT_W_H-100}),
      wxDC:drawLabel(WXPaint, "YellowTeam Score: " ++ integer_to_list(TeamOnePoints) ++ "                   RedTeam Score: " ++ integer_to_list(TeamTwoPoints), {?PRINT_X_START -5*?DOUBLE_TAB, ?PRINT_Y_START + 3*?TAB, ?PRINT_W_H, ?PRINT_W_H}),
      wxDC:drawLabel(WXPaint, "YellowTeam throws:                   RedTeam throws:", {?PRINT_X_START - 5*?DOUBLE_TAB, ?PRINT_Y_START + 5 * ?TAB, ?PRINT_W_H, ?PRINT_W_H}),
      displayShootStats("Player 1   - ", WXPaint, Player1_1, Player2_1, 3 * ?TAB),
      displayShootStats("Player 2   - ", WXPaint, Player1_2, Player2_2, 4 * ?TAB),
      displayShootStats("Player 3   - ", WXPaint, Player1_3, Player2_3, 5 * ?TAB),
      displayShootStats("Player 4   - ", WXPaint, Player1_4, Player2_4, 6 * ?TAB),
      wxDC:drawLabel(WXPaint, "Player 5   - " ++ integer_to_list(Player1_5) ++ "                            Your Player   - "
        ++ integer_to_list(ControlledPlayer), {?PRINT_X_START - 5*?DOUBLE_TAB, ?PRINT_Y_START + 7* ?TAB + ?BIGGER_INDENTATIONS, ?PRINT_W_H, ?PRINT_W_H});
    _ -> continue
  end.


initializeBallLocation() ->
  ets:insert(ball, {ballX, ?BALL_INIT_X}),
  ets:insert(ball, {ballY, ?BALL_INIT_Y}).
  
initializeBallLocation1()->
  [{_,LastScored}] = ets:lookup(ball,lastTeamScored),
  if LastScored =:= 2 ->
    ets:insert(ball, {ballX, 70}),
    ets:insert(ball, {ballY, 400});
  LastScored =:= 1 ->     
    ets:insert(ball, {ballX, 1369}),
    ets:insert(ball, {ballY, 400});
   true -> cont
  end.
  
displayShootStats(PlayerType, WXPaint, PlayerStatistics, Player2Statistics, Indentations) ->
  Text = PlayerType ++ integer_to_list(PlayerStatistics) ++ "                            " ++ PlayerType ++ integer_to_list(Player2Statistics),
  wxDC:drawLabel(WXPaint, Text, {?PRINT_X_START - 5*?DOUBLE_TAB, ?PRINT_Y_START + Indentations + ?BIGGER_INDENTATIONS, ?PRINT_W_H, ?PRINT_W_H}).

winnerTeam() ->
  [{_, TeamOnePoints}] = ets:lookup(stats, teamOnePoints),
  [{_, TeamTwoPoints}] = ets:lookup(stats, teamTwoPoints),
  if (TeamOnePoints > TeamTwoPoints) -> 1;
    true -> 2
  end.
  
 
calcDir(X_1,Y_1,X_2,Y_2,StepSize)->
  Slope = (Y_2 - Y_1) / (X_2 - X_1),
  SlopeSquared = math:pow(Slope, 2),
  StepSquared = math:pow(StepSize, 2),
  Small_X_Step = math:sqrt(StepSquared / (SlopeSquared + 1)),
  Small_Y_Step = Small_X_Step * Slope,
  Direction_X = X_1 - X_2,
  Direction_Y = Y_1 - Y_2,
  {Slope, Direction_Y, Direction_X, Small_Y_Step, Small_X_Step} .

pickArea(X, 4, MonitorsNames) ->
  [Monitor1, Monitor2, Monitor3, Monitor4] = MonitorsNames,
  SectionWidth = getSectionWidth(4),
  case {X < SectionWidth, X < 2 * SectionWidth, X < 3 * SectionWidth, X < 4 * SectionWidth + 100} of
    {true, _, _, _} -> Monitor1;
    {false, true, _, _} -> Monitor2;
    {false, false, true, _} -> Monitor3;
    {false, false, false, true} -> Monitor4
  end;
pickArea(X, 3, MonitorsNames) ->
  [Monitor1, Monitor2, Monitor3] = MonitorsNames,
  SectionWidth = getSectionWidth(3),
  case {X < SectionWidth, X < 2 * SectionWidth, X < 3 * SectionWidth + 100} of
    {true, _, _} -> Monitor1;
    {false, true, _} -> Monitor2;
    {false, false, true} -> Monitor3
  end;
pickArea(X, 2, MonitorsNames) ->
  [Monitor1, Monitor2] = MonitorsNames,
  SectionWidth = getSectionWidth(2),
  case {X < SectionWidth, X < 2 * SectionWidth + 100} of
    {true, _} -> Monitor1;
    {false, true} -> Monitor2
  end;
pickArea(_, 1, MonitorsNames) ->
  [Monitor1] = MonitorsNames,
  Monitor1.

getSectionWidth(MonitorsNumber) ->
  ((?X_Upper_Limit + 1) / MonitorsNumber).

calcDis({X1, Y1}, {X2, Y2}) ->
  DiffX = X1 - X2,
  DiffY = Y1 - Y2,
  DiffX_2 = math:pow(DiffX, 2),
  DiffY_2 = math:pow(DiffY, 2),
  math:sqrt(DiffX_2 + DiffY_2).

movingPlayers({X, Y_1}, {X, Y_2}, StepSize) ->
  RelativeDistance = calcDis({X, Y_1}, {X, Y_2}) - StepSize,
  case RelativeDistance < 0 of
    true -> {X, Y_2};
    _ ->
      case Y_1 > Y_2 of
        true ->
          NextY = Y_1 - StepSize,
          {X, NextY};
        false ->
          NextY = Y_1 + StepSize,
          {X, NextY}
      end
  end;

movingPlayers({X_1, Y_1}, {X_2, Y_2}, StepSize) ->
  RelativeDistance = calcDis({X_1, Y_1}, {X_2, Y_2}) - StepSize,
  case RelativeDistance < 0 of
    true -> {X_2, Y_2};
    _ ->
      {Slope, Direction_Y, Direction_X, Small_Y_Step, Small_X_Step} = calcDir(X_1,Y_1,X_2,Y_2,StepSize),
      if Direction_X < 0 ->
        NextX = X_1 + Small_X_Step;
        true -> NextX = X_1 - Small_X_Step
      end,
      if (((Direction_Y < 0) and (Slope > 0)) or ((Direction_Y > 0) and (Slope < 0))) ->
        NextY = Y_1 + Small_Y_Step,
        {NextX, NextY};
        true ->
          NextY = Y_1 - Small_Y_Step,
          {NextX, NextY}
      end
  end.

moveBall({X, Y_1}, {X, Y_2}, StepSize) ->
  RelativeDistance = calcDis({X, Y_1}, {X, Y_2}) - StepSize,
  case RelativeDistance < 0 of
    true ->
      ets:insert(isOwner, {shootDirection, 0}),
      ets:insert(ball, {flag, false}),
      {X, Y_2};
    _ ->
      case Y_1 > Y_2 of
        true ->
          NextY = Y_1 - StepSize;
        false ->
          NextY = Y_1 + StepSize
      end,
      {X, NextY}
  end;

moveBall({X_1, Y_1}, {X_2, Y_2}, StepSize) ->
  RelativeDistance = calcDis({X_1, Y_1}, {X_2, Y_2}) - StepSize,
  case RelativeDistance < 0 of
    true -> {X_2, Y_2};
    _ ->
      {Slope, Direction_Y, Direction_X, Small_Y_Step, Small_X_Step} = calcDir(X_1,Y_1,X_2,Y_2,StepSize),
      if Direction_X < 0 ->
        NextX = X_1 + Small_X_Step;
        true -> NextX = X_1 - Small_X_Step
      end,
      if (((Direction_Y < 0) and (Slope > 0)) or ((Direction_Y > 0) and (Slope < 0))) ->
        NextY = Y_1 + Small_Y_Step;
        true ->
          NextY = Y_1 - Small_Y_Step
      end,
      {NextX, NextY}
  end.


