%% @author niv
%% @doc @todo Add description to botplayers.


-module(botplayers).

%% ====================================================================
%% API functions
%% ====================================================================
-behaviour(gen_statem).

-include("defineConsts.hrl").

-export([startgen/4, init/1, callback_mode/0, moving/3, delete/1, switchArea/3, terminate/3]).


startgen(ID, Location, Destination, MonitorName) -> %function that starting the gen_statem
  gen_statem:start(?MODULE, [ID, Location, Destination, MonitorName], []).

init([ID, Location, Destination, MonitorName]) -> %initialize the FSM of the bot players after start
  Self = self(),
  FunToSpawn = fun() -> global:register_name(ID, Self) end,
  erlang:link(global:whereis_name(MonitorName)),
  spawn(FunToSpawn),
  put(id, ID),
  put(localsrv, MonitorName),
  put(location, Location),
  put(dest, Destination),
  insertComponent(),
  timer:send_interval(?REFRESH_RATE, self(), refresh),
  loopAction(?WaitingTime, {move, Destination}),
  {ok, moving, {}}.

callback_mode() -> %when need to find the call back mode of the callback module
  state_functions.

delete(ID) ->
  gen_statem:cast({global, ID}, delete).

switchArea(ID, MonitorsNumber, MonitorsNames) -> %for other process/computers that responssible on thier sides
  gen_statem:cast({global, ID}, {switchArea, MonitorsNumber, MonitorsNames}).

insertComponent() ->
  localsrv:insertComponent(get(localsrv), get(id), {computerplayer, get(location), {get(dest)}}).

refreshInfo() ->
  localsrv:refreshInfo(get(localsrv), get(id), {computerplayer, get(location), {get(dest)}}).

moving(_, refresh, Data) -> %function for changing the state of the bot players acording to what happend on field and sending information about it
  refreshInfo(),
  {next_state, moving, Data};

moving(_, {move, Destination}, Data) -> %state of the bot players for moving with or without the ball
  MyId = get(id), NextLocation = helpfunctions:movingPlayers(get(location), Destination, 2),
  put(location, NextLocation), refreshInfo(),
  case NextLocation == Destination of
    false ->
      loopAction(?WaitingTime, {move, Destination});
    _ ->
      {NextX, NextY} = randomDest(MyId),
      put(dest, {NextX, NextY}),
      loopAction(?WaitingTime, {move, {NextX, NextY}})
  end,
  {next_state, moving, Data};

moving(cast, delete, Data) ->
  abortNextAction(),
  {stop, normal, Data};

moving(cast, {switchArea, MonitorsNumber, MonitorsNames}, Data) ->
  abortNextAction(),
  {X, _} = get(location),
  NextOwner = helpfunctions:pickArea(X, MonitorsNumber, MonitorsNames),
  ID = get(id),
  localsrv:switchArea(get(localsrv), ID),
  global:unregister_name(ID),
  localsrv:addBotPlayer(NextOwner, {ID, get(location), get(dest)}),
  {stop, normal, Data}.

loopAction(Time, Content) ->
  NextActionTimerRef = erlang:send_after(Time, self(), Content),
  put(previousAction, Content),
  put(nextActionTimerRef, NextActionTimerRef).

abortNextAction() ->
  erlang:cancel_timer(get(nextActionTimerRef)).

terminate(_Reason, _StateName, _StatData) ->
  ok.

randomDest(ID) -> %deciding the random movments of the bot players on field
  if (ID =< 1.5) ->
    Result = {rand:uniform(350) + 70, rand:uniform(350) + 70};
    true ->
      Result = {rand:uniform(1360) + 120, rand:uniform(460) + 120}
  end,
  Result.

