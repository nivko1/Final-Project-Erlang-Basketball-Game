Final Project - Functional Programming in Concurrent and Distributed Systems

Author: Niv Koren

Professor: Yehuda Ben-Shimol

Teaching Assistant: David Leon

Basketball Game - Distributed and Concurrent (5 LAN computers - 4 local client, 1 local server)

Based 100% erlang code.

# Requirments:

Erlang\OTP 22

# Using:

wx for GUI (wx application is an erlang binding of wxWidgets)

inet for distributed game (module provides access to TCP/IP protocols)

# Architecture:

Two teams (red team and yellow team) of 5 players each.

The user control one player from the red team using I/O to shoot and move.

The other players are bots and moving around the court playing the game.

The game plays 4 quarters, each quarter is played until one team hit the basket.

Each successful hit is 2 points.

# How To Use:

1) Go to defineConsts.hrl file and change the values of the 4 computers according to your computers' names/Ip's.

Example: 

If runnig on Single computer :

		-define(COMPUTER_NAME1, 'local1@niv-VirtualBox').  --> 'niv-VirtualBox' is the computer's names need to be changed to the new computer's name.

If runnig on 5 different computers :

		-define(COMPUTER_NAME1, 'local1@niv-VirtualBox').  --> 'niv-VirtualBox' is the computer's names need to be changed to the computer's IP address.

2) Open 5 terminals: 5 on the same computer if you wish to run Single OR 1 on each computer if you wish to run as distributed (can also use ssh command and run 5 on same computer ).

3) On 4 terminals run the next command : ( x = 1,2,3,4 )

Single:

		erl -sname localx

Distributed:
		
		erl -name localx@<PC-IP> -setcookie choco

4) On the 5th terminal run the next commands:

Single:

		erl -sname mainsrv
		
		c(master).
		
		master:startgen().

Distributed:
		
		erl -name mainsrv@<PC-IP> -smp -setcookie choco
		
		c(master).
		
		master:startgen().


#### The game should now start - enjoy !

YouTube Link:  https://youtu.be/aX76Xs5duNo

