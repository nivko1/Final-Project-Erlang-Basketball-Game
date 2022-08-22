%% @author niv
%% @doc @todo Add description to 


%% ====================================================================
%% API functions
%% ====================================================================

% Times & Computer names configurations
-define(WaitingTime, 45).
-define(REFRESH_RATE, 75).
-define(COMPUTER_NAME1, 'local1@niv-VirtualBox').
-define(COMPUTER_NAME2, 'local2@niv-VirtualBox').
-define(COMPUTER_NAME3, 'local3@niv-VirtualBox').
-define(COMPUTER_NAME4, 'local4@niv-VirtualBox').
-define(PROCESSES_AREA,  [?COMPUTER_NAME1, ?COMPUTER_NAME2, ?COMPUTER_NAME3, ?COMPUTER_NAME4]).
-define(LOCAL1, 'local1').
-define(LOCAL2, 'local2').
-define(LOCAL3, 'local3').
-define(LOCAL4, 'local4').
% Images directory and access.
-define(TEAMYELLOW, "Images/yellow.png").
-define(TEAMRED, "Images/red.png").
-define(BALL_IMAGE, "Images/ball.png").
-define(FINISHED_IMAGE, "Images/lastImage.png").
-define(STAGE_IMAGE, "Images/openImage.png").
-define(COURT_IMAGE, "Images/court.jpg").
% Display on screen.
-define(PRINT_X_START, 400).
-define(PRINT_Y_START, 130).
-define(PRINT_W_H, 300).
-define(TAB, 30).
-define(DOUBLE_TAB, 60).
-define(BIGGER_INDENTATIONS, 120).
% basketball constants.
-define(BALL_DIFF, 20).
-define(BALL_WIDTH_MULTIPLY_FACTOR, 1.35).
-define(BALL_INIT_X, 698).
-define(BALL_INIT_Y, 390).
-define(BALL_INITIAL_COORDINATES, {600, 500}).
-define(BASKET_UP_LIM, 430).
-define(BASKET_LOW_LIM, 389).
% Basket & Field & Players Limitations.
-define(X_Upper_Limit, 1439).
-define(X_Lower_Limit, 20).
-define(Y_Upper_Limit, 750).
-define(Y_Lower_Limit, 15).
