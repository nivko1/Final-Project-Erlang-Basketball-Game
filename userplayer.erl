%% @author niv
%% @doc @todo Add description to userplayer.


-module(userplayer).
-behaviour(gen_statem).
-include("defineConsts.hrl").
-export([init/1, startgen/3, move/2, moveUser/3, delete/1, switchArea/3,  callback_mode/0, terminate/3]).

startgen(ID, Location,MonitorName) -> %function that starting the gen_statem for the user player
  gen_statem:start(?MODULE, [ID, Location, MonitorName], []).

init([ID, Location, MonitorName]) -> %initialize the FSM of the basketball after start
  erlang:link(global:whereis_name(MonitorName)),
  Self = self(),
  FunToSpawn = fun() -> global:register_name(ID, Self) end,
  spawn(FunToSpawn),
  put(id, ID),
  put(localsrv, MonitorName),
  put(location, Location),
  insertComponent(),
  loopAction(?WaitingTime, {move, Location}),
  {ok, moveUser, {}}.

loopAction(Time, Content) ->
  NextActionTimerRef = erlang:send_after(Time, self(), Content),
  put(nextActionTimerRef, NextActionTimerRef).

moveUser(_,{move, Location}, Content) -> %function for changing the state of the the mause user players according to what happend on field and sending information about it
  abortNextAction(),
  put(location, Location),
  refreshInfo(),
  {next_state, moveUser, Content};  %with or without the ball

moveUser(_,delete, Content) ->
  abortNextAction(),
  {stop, normal, Content};

moveUser(_,{switchArea, MonitorsNumber, MonitorsNames}, Content) ->
  abortNextAction(),
  {X, Y} = get(location),
  ID = get(id),
  localsrv:switchArea( get(localsrv), ID), %delete obj from ets @ localsrv
  NextOwner = helpfunctions:pickArea(X, MonitorsNumber, MonitorsNames),
  global:unregister_name(ID),
  localsrv:addUser(NextOwner, {ID, {X,Y}, old}),
  {stop, normal, Content}.
  
move(ID, Location) ->
  gen_statem:cast({global, ID}, {move, Location}).

delete(ID) ->  %function build for the gen_starem
  gen_statem:cast({global, ID}, delete).

switchArea(ID, MonitorsNumber, MonitorsNames) ->
  gen_statem:cast({global, ID}, {switchArea, MonitorsNumber, MonitorsNames}).

abortNextAction() ->
  erlang:cancel_timer(get(nextActionTimerRef)).

terminate(_Reason, _StateName, _StatData) ->
  ok.

refreshInfo() ->
  localsrv:refreshInfo(get(localsrv), get(id), {userplayer, get(location), { get(localsrv)}}).

insertComponent() ->
  localsrv:insertComponent(get(localsrv), get(id), {userplayer, get(location), {get(localsrv)}}).

callback_mode() ->
  state_functions.
