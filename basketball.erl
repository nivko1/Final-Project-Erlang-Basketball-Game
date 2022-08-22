%% @author niv
%% @doc @todo Add description to basketball


-module(basketball).

%% ====================================================================
%% API functions
%% ====================================================================

-behaviour(gen_statem).
-include("defineConsts.hrl").
-export([startgen/4, init/1, callback_mode/0, moving/3, delete/1, switchArea/3, terminate/3]).


startgen(ID, Location, Destination, MonitorName) -> %function that starting the gen_statem
  gen_statem:start(?MODULE, [ID, Location, Destination, MonitorName], []).

init([ID, Location, Destination, MonitorName]) -> %initialize the FSM of the basketball after start
  Self = self(),
  FunToSpawn = fun() -> global:register_name(ID, Self) end,
  erlang:link(global:whereis_name(MonitorName)), %linked with the local server process
  spawn(FunToSpawn),
  put(id, ID), %put id and other parameters in dictionary
  put(localsrv, MonitorName),
  put(location, Location),
  put(dest, Destination),
  insertComponent(), %calling function
  timer:send_interval(?REFRESH_RATE, Self, refresh), %sending message - to self (basket procces) after time 
  loopAction(?WaitingTime, {move, Destination}),  %entering loop action
  {ok, moving, {}}.
  

moving(_, refresh, Data) -> %function for changing the state of the ball process and sending information about it
  refreshInfo(),
  {next_state, moving, Data};

moving(_, {move, Destination}, Data) ->
  NextLocation = helpfunctions:movingPlayers(get(location), Destination, 2),
  put(location, NextLocation),
  refreshInfo(),
  loopAction(?WaitingTime, {move, Destination}),
  {next_state, moving, Data};

moving(cast, delete, Data) ->
  abortNextAction(),
  global:unregister_name(get(id)),
  {stop, normal, Data};
  
moving(cast, {switchArea, MonitorsNumber, MonitorsNames}, Data) ->
  abortNextAction(),
  {X, _} = get(location),
  NextOwner = helpfunctions:pickArea(X, MonitorsNumber, MonitorsNames),
  ID = get(id),
  localsrv:switchArea(get(localsrv), ID),
  global:unregister_name(ID),
  localsrv:addBall(NextOwner, {ID, get(location), get(dest), old}),
  {stop, normal, Data}.
  
%some function build in for the gen_statem

abortNextAction() -> 
      erlang:cancel_timer(get(nextActionTimerRef)).
      
      
loopAction(Time, Content) -> %if something happed we saved the dest and the current pos
  NextActionTimerRef = erlang:send_after(Time, self(), Content),
  put(previousAction, Content),
  put(nextActionTimerRef, NextActionTimerRef).

callback_mode() ->
  state_functions.
  

delete(ID) ->
  gen_statem:cast({global, ID}, delete).
  

switchArea(ID, MonitorsNumber, MonitorsNames) -> %for other process/computers that responssible on thier sides
  gen_statem:cast({global, ID}, {switchArea, MonitorsNumber, MonitorsNames}).
  

insertComponent() -> %calling to fuucn on localsrv
  localsrv:insertComponent(get(localsrv), get(id), {ball, get(location), {get(dest)}}).
  

refreshInfo() -> %calling to fuucn on localsrv
  localsrv:refreshInfo(get(localsrv), get(id), {ball, get(location), {get(dest)}}).
  

terminate(_Reason, _StateName, _StatData) ->
  ok.

