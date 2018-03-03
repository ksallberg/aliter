-module(db).

-include("records.hrl").

-export([ ping/1 ]).

-export([ save_account/2
        , get_account/2
        , get_account_id/2 ]).

-export([ save_char/2
        , delete_char/2
        , get_char/2
        , get_account_chars/2
        , get_account_char/3
        , get_char_id/2
        , rename_char/4 ]).

-export([ save_guild/2
        , delete_guild/2
        , get_guild/2
        , get_guild_id/2
        , get_guild_master/2
        , get_guild_members/2
        , add_char_to_guild/3
        , delete_char_from_guild/3
        , get_guild_relationships/2 ]).

-export([ save_guild_relationship/4
        , delete_guild_relationship/3
        , get_guild_relationship/3 ]).

-export([ give_world_item/4
        , get_world_items/2
        , get_world_item/2
        , remove_world_item/3 ]).

-export([ give_player_item/4
        , get_player_items/2
        , get_player_item/3
        , remove_player_item/3 ]).

save_account(C, Account) ->
    ID =
        case Account#account.id of
            undefined -> incr(C, "accounts:id");
            X -> X
        end,
    lager:log(info, self(), "Creating account ~p ~p",
              [{id, ID}, {account_id, Account#account.id}]),
    Hash = "account:" ++ integer_to_list(ID),
    sethash(C, Hash, "login", Account#account.login),
    sethash(C, Hash, "password", Account#account.password),
    sethash(C, Hash, "email", Account#account.email),
    sethash(C, Hash, "gender", Account#account.gender),
    sethash(C, Hash, "login_count", Account#account.login_count),
    sethash(C, Hash, "last_login", Account#account.last_login),
    sethash(C, Hash, "last_ip", Account#account.last_ip),
    sethash(C, Hash, "gm_level", Account#account.gm_level),
    set(C, ["account:", Account#account.login], ID),
    Account#account{id = ID}.

get_account(C, ID) ->
    Hash = ["account:", integer_to_list(ID)],
    #account{
       id = ID,
       login = gethash(C, Hash, "login"),
       password = gethash(C, Hash, "password"),
       email = gethash(C, Hash, "email"),
       gender = numeric(gethash(C, Hash, "gender")),
       login_count = numeric(gethash(C, Hash, "login_count")),
       last_login = numeric(gethash(C, Hash, "last_login")),
       last_ip = gethash(C, Hash, "last_ip"),
       gm_level = numeric(gethash(C, Hash, "gm_level"))}.

get_account_id(C, Name) ->
    case db_get(C, ["account:", Name]) of
        undefined -> nil;
        {ok, X} ->
            numeric(X)
    end.

save_char(C, Char) ->
    ID =
        case Char#char.id of
            undefined -> incr(C, "chars:id");
            X -> X
        end,
    Hash = "char:" ++ integer_to_list(ID),
    sethash(C, Hash, "num", Char#char.num),
    sethash(C, Hash, "name", Char#char.name),
    sethash(C, Hash, "job", Char#char.job),
    sethash(C, Hash, "base_level", Char#char.base_level),
    sethash(C, Hash, "base_exp", Char#char.base_exp),
    sethash(C, Hash, "job_level", Char#char.job_level),
    sethash(C, Hash, "job_exp", Char#char.job_exp),
    sethash(C, Hash, "zeny", Char#char.zeny),
    sethash(C, Hash, "str", Char#char.str),
    sethash(C, Hash, "agi", Char#char.agi),
    sethash(C, Hash, "vit", Char#char.vit),
    sethash(C, Hash, "int", Char#char.int),
    sethash(C, Hash, "dex", Char#char.dex),
    sethash(C, Hash, "luk", Char#char.luk),
    sethash(C, Hash, "max_hp", Char#char.max_hp),
    sethash(C, Hash, "hp", Char#char.hp),
    sethash(C, Hash, "max_sp", Char#char.max_sp),
    sethash(C, Hash, "sp", Char#char.sp),
    sethash(C, Hash, "status_points", Char#char.status_points),
    sethash(C, Hash, "skill_points", Char#char.skill_points),
    sethash(C, Hash, "hair_style", Char#char.hair_style),
    sethash(C, Hash, "hair_colour", Char#char.hair_colour),
    sethash(C, Hash, "clothes_colour", Char#char.clothes_colour),
    sethash(C, Hash, "view_weapon", Char#char.view_weapon),
    sethash(C, Hash, "view_shield", Char#char.view_shield),
    sethash(C, Hash, "view_head_top", Char#char.view_head_top),
    sethash(C, Hash, "view_head_middle", Char#char.view_head_middle),
    sethash(C, Hash, "view_head_bottom", Char#char.view_head_bottom),
    sethash(C, Hash, "map", Char#char.map),
    sethash(C, Hash, "x", Char#char.x),
    sethash(C, Hash, "y", Char#char.y),
    sethash(C, Hash, "save_map", Char#char.save_map),
    sethash(C, Hash, "save_x", Char#char.save_x),
    sethash(C, Hash, "save_y", Char#char.save_y),
    sethash(C, Hash, "online", Char#char.online),
    sethash(C, Hash, "effects", Char#char.effects),
    sethash(C, Hash, "karma", Char#char.karma),
    sethash(C, Hash, "manner", Char#char.manner),
    sethash(C, Hash, "fame", Char#char.fame),
    sethash(C, Hash, "guild_position", Char#char.guild_position),
    sethash(C, Hash, "guild_taxed", Char#char.guild_taxed),
    sethash(C, Hash, "renamed", Char#char.renamed),
    sethash(C, Hash, "account_id", Char#char.account_id),
    sethash(C, Hash, "party_id", Char#char.party_id),
    sethash(C, Hash, "guild_id", Char#char.guild_id),
    sethash(C, Hash, "pet_id", Char#char.pet_id),
    sethash(C, Hash, "homunculus_id", Char#char.homunculus_id),
    sethash(C, Hash, "mercenary_id", Char#char.mercenary_id),
    set(C, ["char:", Char#char.name], ID),
    sethash(
      C,
      ["account:", integer_to_list(Char#char.account_id), ":chars"],
      Char#char.num,
      ID
     ),
    Char#char{id = ID}.

delete_char(C, Char) ->
    Hash = "char:" ++ integer_to_list(Char#char.id),
    delete(C, Hash),
    delete(C, ["char:", Char#char.name]),
    hdel(C, ["account:", integer_to_list(Char#char.account_id), ":chars"],
         Char#char.num),
    ok.

get_char(C, ID) ->
    Hash = "char:" ++ integer_to_list(ID),
    #char{
       id = ID,
       num = numeric(gethash(C, Hash, "num")),
       name = gethash(C, Hash, "name"),
       job = numeric(gethash(C, Hash, "job")),
       base_level = numeric(gethash(C, Hash, "base_level")),
       base_exp = numeric(gethash(C, Hash, "base_exp")),
       job_level = numeric(gethash(C, Hash, "job_level")),
       job_exp = numeric(gethash(C, Hash, "job_exp")),
       zeny = numeric(gethash(C, Hash, "zeny")),
       str = numeric(gethash(C, Hash, "str")),
       agi = numeric(gethash(C, Hash, "agi")),
       vit = numeric(gethash(C, Hash, "vit")),
       int = numeric(gethash(C, Hash, "int")),
       dex = numeric(gethash(C, Hash, "dex")),
       luk = numeric(gethash(C, Hash, "luk")),
       max_hp = numeric(gethash(C, Hash, "max_hp")),
       hp = numeric(gethash(C, Hash, "hp")),
       max_sp = numeric(gethash(C, Hash, "max_sp")),
       sp = numeric(gethash(C, Hash, "sp")),
       status_points = numeric(gethash(C, Hash, "status_points")),
       skill_points = numeric(gethash(C, Hash, "skill_points")),
       hair_style = numeric(gethash(C, Hash, "hair_style")),
       hair_colour = numeric(gethash(C, Hash, "hair_colour")),
       clothes_colour = numeric(gethash(C, Hash, "clothes_colour")),
       view_weapon = numeric(gethash(C, Hash, "view_weapon")),
       view_shield = numeric(gethash(C, Hash, "view_shield")),
       view_head_top = numeric(gethash(C, Hash, "view_head_top")),
       view_head_middle = numeric(gethash(C, Hash, "view_head_middle")),
       view_head_bottom = numeric(gethash(C, Hash, "view_head_bottom")),
       map = gethash(C, Hash, "map"),
       x = numeric(gethash(C, Hash, "x")),
       y = numeric(gethash(C, Hash, "y")),
       save_map = gethash(C, Hash, "save_map"),
       save_x = numeric(gethash(C, Hash, "save_x")),
       save_y = numeric(gethash(C, Hash, "save_y")),
       online = numeric(gethash(C, Hash, "online")),
       effects = numeric(gethash(C, Hash, "effects")),
       karma = numeric(gethash(C, Hash, "karma")),
       manner = numeric(gethash(C, Hash, "manner")),
       fame = numeric(gethash(C, Hash, "fame")),
       guild_position = numeric(gethash(C, Hash, "guild_position")),
       guild_taxed = numeric(gethash(C, Hash, "guild_taxed")),
       renamed = numeric(gethash(C, Hash, "renamed")),
       account_id = numeric(gethash(C, Hash, "account_id")),
       party_id = numeric(gethash(C, Hash, "party_id")),
       guild_id = numeric(gethash(C, Hash, "guild_id")),
       pet_id = numeric(gethash(C, Hash, "pet_id")),
       homunculus_id = numeric(gethash(C, Hash, "homunculus_id")),
       mercenary_id = numeric(gethash(C, Hash, "mercenary_id"))}.

get_account_chars(C, AccountID) ->
    Chars = hgetall(C, ["account:", integer_to_list(AccountID), ":chars"]),
    [get_char(C, numeric(ID)) || {_, ID} <- Chars].

get_account_char(C, AccountID, Num) ->
    Path = ["account:", integer_to_list(AccountID), ":chars"],
    ID = hget(C, Path, integer_to_list(Num)),
    case ID of
        undefined -> nil;
        {ok, X} -> get_char(C, numeric(X))
    end.

get_char_id(C, Name) ->
    case db_get(C, ["char:", Name]) of
        undefined -> nil;
        {ok, X} -> numeric(X)
    end.

rename_char(C, ID, OldName, NewName) ->
    Hash = "char:" ++ integer_to_list(ID),
    delete(C, ["char:", OldName]),
    set(C, ["char:", NewName], ID),
    sethash(C, Hash, "name", NewName),
    sethash(C, Hash, "renamed", 1),
    ok.

save_guild(C, Guild) ->
    ID =
        case Guild#guild.id of
            undefined -> incr(C, "guilds:id");
            X -> X
        end,
    Hash = "guild:" ++ integer_to_list(ID),
    sethash(C, Hash, "name", Guild#guild.name),
    sethash(C, Hash, "level", Guild#guild.level),
    sethash(C, Hash, "capacity", Guild#guild.capacity),
    sethash(C, Hash, "exp", Guild#guild.exp),
    sethash(C, Hash, "next_exp", Guild#guild.next_exp),
    sethash(C, Hash, "skill_points", Guild#guild.skill_points),
    sethash(C, Hash, "message_title", Guild#guild.message_title),
    sethash(C, Hash, "message_body", Guild#guild.message_body),
    sethash(C, Hash, "emblem", Guild#guild.emblem),
    sethash(C, Hash, "master_id", Guild#guild.master_id),
    set(C, ["guild:", Guild#guild.name], ID),
    Guild#guild{id = ID}.

delete_guild(C, Guild) ->
    Hash = "guild:" ++ integer_to_list(Guild#guild.id),
    delete(C, Hash),
    delete(C, ["guild:", Guild#char.name]),
    ok.

get_guild(C, ID) ->
    Hash = "guild:" ++ integer_to_list(ID),
    #guild{id = ID,
           name = gethash(C, Hash, "name"),
           level = numeric(gethash(C, Hash, "level")),
           capacity = numeric(gethash(C, Hash, "capacity")),
           exp = numeric(gethash(C, Hash, "exp")),
           next_exp = numeric(gethash(C, Hash, "next_exp")),
           skill_points = numeric(gethash(C, Hash, "skill_points")),
           message_title = gethash(C, Hash, "message_title"),
           message_body = gethash(C, Hash, "message_body"),
           emblem = gethash(C, Hash, "emblem"),
           master_id = numeric(gethash(C, Hash, "master_id"))}.

get_guild_id(C, Name) ->
    case db_get(C, ["guild:", Name]) of
        undefined -> nil;
        {ok, X} -> numeric(X)
    end.

get_guild_master(C, GuildId) ->
    ID =
        case GuildId of
            undefined -> incr(C, "guilds:id");
            X -> X
        end,
    Hash = "guild:" ++ integer_to_list(ID),
    case hget(C, Hash, "master_id") of
        undefined -> nil;
        {ok, Master} -> numeric(Master)
    end.

get_guild_members(C, GuildID) ->
    Chars = lrange(C, ["guild:", integer_to_list(GuildID), ":members"], 0, -1),
    [get_char(C, numeric(ID)) || {_, ID} <- Chars].

add_char_to_guild(C, GuildID, CharacterID) ->
    rpush(C, ["guild:", GuildID, ":members"], CharacterID).

delete_char_from_guild(C, GuildID, CharacterID) ->
    lrem(C, ["guild:", GuildID, ":members"], 0, CharacterID),
    ok.

get_guild_relationships(C, GuildID) ->
    hgetall(C, ["guild:", integer_to_list(GuildID), ":relationships"]).

save_guild_relationship(C, GuildID, TargetID, Type) ->
    Key = ["guild:", integer_to_list(GuildID), ":relationships"],
    sethash(C, Key, integer_to_list(TargetID), integer_to_list(Type)).

delete_guild_relationship(C, GuildID, TargetID) ->
    hdel(C, ["guild:", integer_to_list(GuildID), ":relationships"],
         integer_to_list(TargetID)),
    ok.

get_guild_relationship(C, GuildID, TargetID) ->
    numeric(
      gethash(C,
              [ "guild:",
                integer_to_list(GuildID),
                ":relationships"],
              integer_to_list(TargetID))).

give_world_item(C, Map, ItemID, Amount) ->
    ObjectID = incr(C, "objects:next"),
    sadd(C, ["objects:", Map], ObjectID),
    sethash(C, ["objects:", integer_to_list(ObjectID)], "item", ItemID),
    sethash(C, ["objects:", integer_to_list(ObjectID)], "amount", Amount),
    ObjectID.

get_world_items(C, Map) ->
    Items = smembers(C, ["objects:", Map]),
    [get_world_item(C, numeric(Slot)) || Slot <- Items].

get_world_item(C, ObjectID) ->
    Object = ["objects:", integer_to_list(ObjectID)],
    case hget(C, Object, "item") of
        {ok, Item} ->
            case hget(C, Object, "amount") of
                {ok, Amount} ->
                    #world_item{
                       slot = ObjectID,
                       item = numeric(Item),
                       amount = numeric(Amount)};

                undefined -> nil
            end;

        undefined -> nil
    end.

remove_world_item(C, Map, ObjectID) ->
    srem(C, ["objects:", Map], integer_to_list(ObjectID)),
    delete(C, ["objects:", integer_to_list(ObjectID)]).

give_player_item(C, CharacterID, ItemID, Amount) ->
    Slot = incr(C, ["inventory:", integer_to_list(CharacterID), ":next"]),
    sadd(C, ["inventory:", integer_to_list(CharacterID)], Slot),
    sethash(C, ["inventory:", integer_to_list(CharacterID),
                ":", integer_to_list(Slot)], "item", ItemID),
    sethash(C, ["inventory:", integer_to_list(CharacterID),
                ":", integer_to_list(Slot)], "amount", Amount),
    Slot.

get_player_items(C, CharacterID) ->
    Items = smembers(C, ["inventory:", integer_to_list(CharacterID)]),
    [get_player_item(C, CharacterID, numeric(Slot)) || Slot <- Items].

get_player_item(C, CharacterID, Slot) ->
    Object = [
              "inventory:",
              integer_to_list(CharacterID), ":",
              integer_to_list(Slot)
             ],
    case hget(C, Object, "item") of
        {ok, Item} ->
            case hget(C, Object, "amount") of
                {ok, Amount} ->
                    #world_item{
                       slot = Slot,
                       item = numeric(Item),
                       amount = numeric(Amount)};

                undefined -> nil
            end;

        undefined -> nil
    end.

remove_player_item(C, CharacterID, Slot) ->
    Inventory = ["inventory:", integer_to_list(CharacterID)],
    srem(C, Inventory, integer_to_list(Slot)),
    delete(C, [Inventory, ":", integer_to_list(Slot)]),
    ok.

gethash(C, Key, Field) ->
    case hget(C, Key, Field) of
        undefined -> error(["undefined", Key, Field]);
        {ok, V} -> V
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Facade to whatever redis lib used: %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

incr(C, ToIncr) ->
    {ok, X} = eredis:q(C, ["INCR", ToIncr]),
    list_to_integer(binary_to_list(X)).

sethash(C, Key, Field, Value) ->
    eredis:q(C, ["HSET", Key, Field, Value]).

delete(C, What) ->
    eredis:q(C, ["DEL", What]).

sadd(C, Path, What) ->
    eredis:q(C, ["SADD", Path, What]).

set(C, Path, What) ->
    eredis:q(C, ["SET", Path, What]).

srem(C, Path, What) ->
    eredis:q(C, ["SREM", Path, What]).

hget(C, Path, What) ->
    eredis:q(C, ["HGET", Path, What]).

hgetall(C, Path) ->
    {ok, X} = eredis:q(C, ["HGETALL", Path]),
    multi_hashlist(X).

%% stolen from erlang-redis:
multi_hashlist(Values) ->
    multi_hashlist(Values, []).
multi_hashlist([], Acc) -> lists:reverse(Acc);
multi_hashlist([Name, Val|Rest], Acc) ->
    multi_hashlist(Rest, [{Name, Val}|Acc]).

hdel(C, Path, What) ->
    eredis:q(C, ["HDEL", Path, What]).

db_get(C, What) ->
    case eredis:q(C, ["GET", What]) of
        {ok, undefined} ->
            undefined;
        Other ->
            Other
    end.

lrange(C, Path, Id, Def) ->
    {ok, X} = eredis:q(C, ["LRANGE", Path, Id, Def]),
    X.

lrem(C, Path, Def, CharacterID) ->
    eredis:q(C, ["LREM", Path, Def, CharacterID]).

rpush(C, Path, What) ->
    eredis:q(C, ["RPUSH", Path, What]).

smembers(C, Path) ->
    {ok, X} = eredis:q(C, ["SMEMBERS", Path]),
    X.

ping(C) ->
    eredis:q(C, ["PING"]).

%% TODO: this is probably only called with one form
numeric(I) when is_binary(I) ->
    II = binary_to_list(I),
    try
        list_to_integer(II)
    catch
        error:badarg ->
            try list_to_float(II)
            catch error:badarg -> I
            end
    end;
numeric(I) when is_list(I) ->
    try
        list_to_integer(I)
    catch
        error:badarg ->
            try list_to_float(I)
            catch error:badarg -> I
            end
    end.
