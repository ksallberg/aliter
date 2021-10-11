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

-define(SP_STR,               13).
-define(SP_AGI,               14).
-define(SP_VIT,               15).
-define(SP_INT,               16).
-define(SP_DEX,               17).
-define(SP_LUK,               18).
-define(SP_ATTACKRANGE,       1000).
-define(SP_ASPD,              53).

-define(SP_BASEEXP,           1).
-define(SP_NEXTBASEEXP,       22).
-define(SP_JOBEXP,            2).
-define(SP_NEXTJOBEXP,        23).
-define(SP_SKILLPOINT,        12).
-define(SP_BASELEVEL,         11).
-define(SP_JOBLEVEL,          55).

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

-define(IT_HEALING, 0).
-define(IT_UNKNOWN, 1).
-define(IT_USABLE, 2).
-define(IT_ETC,     3).
-define(IT_WEAPON,  4).
-define(IT_ARMOR,   5).
-define(IT_CARD,    6).
-define(IT_PETEGG,  7).
-define(IT_PETARMOR,8).
-define(IT_UNKNOWN2,9).
-define(IT_AMMO,    10).
-define(IT_DELAYCONSUME, 11).
-define(IT_CASH, 18).
