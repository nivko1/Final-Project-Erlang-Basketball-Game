%% @author niv
%% @doc @todo Add description to etsTables.


-module(etsTables).

%% ====================================================================
%% API functions
%% ====================================================================

-include("defineConsts.hrl").

-export([initEts/0]).
%The main database - for saving the statistics and runtime data and all componnents positions along the games
initEts() -> %function the open new ets tables for sharing processes information (data base)
  ets:new(players, [set, public, named_table]), ets:new(computers, [set, public, named_table]), 
  ets:new(generalState, [set, public, named_table]),
  ets:new(userplayerDir, [set, public, named_table]), ets:new(userplayerX, [set, public, named_table]),
  ets:insert(userplayerDir, {direction, 0}), ets:insert(userplayerX, {xCoordinate, 1}),
  ets:new(ball, [set, public, named_table]), helpfunctions:initializeBallLocation(),
  ets:insert(ball, {previousOwner, 0}), ets:insert(ball, {lastTeamScored,0}),
  ets:new(isOwner, [set, public, named_table]), ets:insert(isOwner, {ball, 0}),
  ets:insert(isOwner, {shootDirection, 0}), ets:insert(isOwner, {owned, 0}),
  ets:new(stats, [set, public, named_table]),
  ets:insert(stats, {1.1, 0}), ets:insert(stats, {1.2, 0}),
  ets:insert(stats, {1.3, 0}), ets:insert(stats, {1.4, 0}),
  ets:insert(stats, {1.5, 0}), ets:insert(stats, {2.1, 0}), ets:insert(stats, {2.2, 0}),
  ets:insert(stats, {2.3, 0}), ets:insert(stats, {2.4, 0}),
  ets:insert(stats, {controlledPlayer, 0}), ets:insert(stats, {prevStatupdate, -1}), 
  ets:insert(stats, {teamOnePoints, 0}), ets:insert(stats, {teamTwoPoints, 0}),
  ets:new(playerDirection, [set, public, named_table]), ets:insert(playerDirection, {1.1, 1}),
  ets:insert(playerDirection, {1.2, 1}), ets:insert(playerDirection, {1.3, 1}),
  ets:insert(playerDirection, {1.4, 1}), ets:insert(playerDirection, {1.5, 1}),
  ets:insert(playerDirection, {2.1, 1}), ets:insert(playerDirection, {2.2, 1}),
  ets:insert(playerDirection, {2.3, 1}), ets:insert(playerDirection, {2.4, 1}),
  ets:insert(generalState, {status, waitForStartGame}), ets:insert(generalState, {quarter, 0}).
