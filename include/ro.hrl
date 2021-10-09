%% -define(PACKETVER,            20111116).
-define(PACKETVER,            20180418).

%% This depends on if you want to run the client
%% on the same machine as the server or not.

%% Same machine = localhost, otherwise the interface's IP you want to use

%% -define(DEBUG_IP,             {127, 0, 0, 1}).
-define(DEBUG_IP,             {10, 0, 1, 185}).

%% Login server
-define(LOGIN_PORT,           6900).

%% Char server
-define(CHAR_SERVER_NAME,     "Aliter").
-define(CHAR_IP,              ?DEBUG_IP).
-define(CHAR_PORT,            5121).

%% Zone server
-define(ZONE_IP,              ?DEBUG_IP).
-define(ZONE_PORT,            9999).

-define(SP_CUR_HP,            5).
-define(SP_MAX_HP,            6).
-define(SP_CUR_SP,            7).
-define(SP_MAX_SP,            8).
-define(SP_ZENY,              20).

-define(BDT_NORMAL,           0).
-define(BDT_ENDURE,           4).
-define(BDT_SPLASH,           5).
-define(BDT_SKILL,            6).
-define(BDT_MULTIHIT,         8).
-define(BDT_MULTIHITENDURE,   9).
-define(BDT_CRIT,             10).
-define(BDT_PDODGE,           11).

%% out of sight
-define(VANISH_OOS,           0).
-define(VANISH_DIED,          1).
-define(VANISH_LOGGED_OUT,    2).
-define(VANISH_TELEPORT,      3).
-define(VANISH_TRICKDEAD,     4).
