-module(db).

-include("records.hrl").

-export([ init/0 ]).

-export([ save_account/1
        , get_account/1
        , get_account_id/1 ]).

-export([ save_char/1
        , delete_char/1
        , get_char/1
        , get_account_chars/1
        , get_account_char/2
        , get_char_id/1
        , rename_char/2 ]).

-export([ save_guild/1
        , delete_guild/1
        , get_guild/1
        , get_guild_id/1
        , get_guild_master/1
        , get_guild_members/1
        , add_char_to_guild/2 ]).

-export([ give_world_item/5
        , get_world_items/1
        , get_world_item/2
        , remove_world_item/2 ]).

-export([ give_player_item/4
        , get_player_items/1
        , get_player_item/2
        , remove_player_item/2 ]).

-export([ % get_equips/2,
          save_equips_ext/2 ]).

-export([ get_mob_data/1,
          get_item_data/1
        ]).

init() ->
    NodeList = [node()],
    Root = filename:absname_join(filename:dirname(?FILE), ".."),
    application:set_env(mnesia, dir, filename:join(Root, "Mnesia.ragnadb")),
    mnesia:create_schema(NodeList),
    mnesia:start(),
    mnesia:create_table(account,
                        [{attributes, record_info(fields, account)},
                         {disc_copies, NodeList}]),
    mnesia:create_table(char,
                        [{attributes, record_info(fields, char)},
                         {disc_copies, NodeList}]),
    mnesia:create_table(inventory,
                        [{attributes, record_info(fields, inventory)},
                         {disc_copies, NodeList}]),

    mnesia:create_table(map_items,
                        [{attributes, record_info(fields, map_items)},
                         {disc_copies, NodeList}]),

    mnesia:create_table(guild,
                        [{attributes, record_info(fields, guild)},
                         {disc_copies, NodeList}]),
    mnesia:create_table(guild_castle,
                        [{attributes, record_info(fields, guild_castle)},
                         {disc_copies, NodeList}]),
    mnesia:create_table(guild_expulsion,
                        [{attributes, record_info(fields, guild_expulsion)},
                         {disc_copies, NodeList}]),
    mnesia:create_table(guild_position,
                        [{attributes, record_info(fields, guild_position)},
                         {disc_copies, NodeList}]),
    mnesia:create_table(guild_relationship,
                        [{attributes, record_info(fields, guild_relationship)},
                         {disc_copies, NodeList}]),
    mnesia:create_table(mob_data,
                        [{attributes, record_info(fields, mob_data)},
                         {disc_copies, NodeList}]),
    mnesia:create_table(item_data,
                        [{attributes, record_info(fields, item_data)},
                         {disc_copies, NodeList}]).

save_account(#account{} = Account) ->
    A = fun() ->
                mnesia:table_info(account, size)
        end,
    {atomic, Len} = mnesia:transaction(A),
    ID =
        case Account#account.id of
            undefined -> Len;
            X -> X
        end,
    NewAccount = Account#account{id = ID},
    Fun = fun() ->
              mnesia:write(NewAccount)
          end,
    mnesia:transaction(Fun),
    NewAccount.

get_account(ID) ->
    F = fun() ->
                MatchHead = #account{id = '$1',
                                     _ = '_'},
                Guards = [{'==', '$1', ID}],
                Result = '$_',
                mnesia:select(account, [{MatchHead,
                                         Guards,
                                         [Result]
                                        }])
        end,
    {atomic, [X]} = mnesia:transaction(F),
    X.

get_account_id(Name) ->
    F = fun() ->
                MatchHead = #account{login = '$1',
                                     id= '$2',
                                     _ = '_'},
                Guards = [{'==', '$1', Name}],
                Result = '$2',
                mnesia:select(account, [{MatchHead,
                                         Guards,
                                         [Result]
                                        }])
        end,
    {atomic, X} = mnesia:transaction(F),
    case X of
        [] ->
            nil;
        [Something] ->
            Something
    end.

save_char(#char{id=CharId} = Char) ->
    NewChar =
        case CharId of
            undefined ->
                A = fun() ->
                            mnesia:table_info(char, size)
                    end,
                {atomic, Len} = mnesia:transaction(A),
                Eq1 = #equip{index = 0,
                             id = 1201,
                             type = 5,
                             identified = 1,
                             location = 2,
                             wearstate = 2,
                             is_damaged = 0,
                             refining_level = 0,
                             card1 = 0,
                             card2 = 0,
                             card3 = 0,
                             card4 = 0,
                             hire_expire_date = 0,
                             bind_on_equip_type = 0,
                             sprite_number = 0},
                Eq2 = #equip{index = 3,
                             id = 2376,
                             type = 4,
                             identified = 1,
                             location = 16,
                             wearstate = 16,
                             is_damaged = 0,
                             refining_level = 0,
                             card1 = 0,
                             card2 = 0,
                             card3 = 0,
                             card4 = 0,
                             hire_expire_date = 0,
                             bind_on_equip_type = 0,
                             sprite_number = 0},
                Eq3 = #equip{index = 5,
                             id = 5025,
                             type = 4,
                             identified = 1,
                             location = 256,
                             wearstate = 256,
                             is_damaged = 0,
                             refining_level = 0,
                             card1 = 0,
                             card2 = 0,
                             card3 = 0,
                             card4 = 0,
                             hire_expire_date = 0,
                             bind_on_equip_type = 0,
                             sprite_number = 110},
                Char#char{id=Len, equips=[Eq1, Eq2, Eq3]};
            _X ->
                Char
        end,
    Fun = fun() ->
                  mnesia:write(NewChar)
          end,
    mnesia:transaction(Fun),
    NewChar.

save_equips_ext(#char{} = Char, #equip{} = Equip) ->
    save_equips(Char, Equip).

save_equips(#char{equips=OldEquips} = Char,
            #equip{index=Ind}=Eq) ->
    NewEquips = lists:keystore(Ind, #equip.index, OldEquips, Eq),
    NewChar = Char#char{equips=NewEquips},
    save_char(NewChar).

delete_char(#char{} = Char) ->
    mnesia:delete_object(Char).

get_char(ID) ->
    F = fun() ->
                MatchHead = #char{id = '$1',
                                  _ = '_'},
                Guards = [{'==', '$1', ID}],
                Result = '$_',
                mnesia:select(char, [{MatchHead,
                                      Guards,
                                      [Result]
                                     }])
        end,
    {atomic, [X]} = mnesia:transaction(F),
    X.

get_account_chars(AccountID) ->
    F = fun() ->
                MatchHead = #char{account_id = '$1',
                                  _ = '_'},
                Guards = [{'==', '$1', AccountID}],
                Result = '$_',
                mnesia:select(char, [{MatchHead,
                                      Guards,
                                      [Result]
                                     }])
        end,
    {atomic, Chars} = mnesia:transaction(F),
    SortedChars = lists:keysort(#char.id, Chars),
    SortedChars.

get_account_char(AccountID, Num) ->
    Chars = get_account_chars(AccountID),
    try
        lists:nth(Num+1, Chars)
    catch _:_ ->
            nil
    end.

get_char_id(Name) ->
    F = fun() ->
                MatchHead = #char{name = '$1',
                                  id= '$2',
                                  _ = '_'},
                Guards = [{'==', '$1', Name}],
                Result = '$2',
                mnesia:select(char, [{MatchHead,
                                      Guards,
                                      [Result]
                                     }])
        end,
    {atomic, X} = mnesia:transaction(F),
    case X of
        [] ->
            nil;
        [Something] ->
            Something
    end.

rename_char(#char{}=Char, NewName) ->
    NewChar = Char#char{name=NewName},
    save_char(NewChar).

save_guild(#guild{} = Guild) ->
    ID =
        case Guild#guild.id of
            undefined ->
                A = fun() ->
                            mnesia:table_info(guild, size)
                    end,
                {atomic, Len} = mnesia:transaction(A),
                Len+1;
            X -> X
        end,
    NewGuild = Guild#guild{id = ID},
    Fun = fun() ->
                  mnesia:write(NewGuild)
          end,
    mnesia:transaction(Fun),
    NewGuild.

delete_guild(#guild{} = Guild) ->
    mnesia:delete_object(Guild).

get_guild(ID) ->
    F = fun() ->
                MatchHead = #guild{id = '$1',
                                   _ = '_'},
                Guards = [{'==', '$1', ID}],
                Result = '$_',
                mnesia:select(guild, [{MatchHead,
                                       Guards,
                                       [Result]
                                      }])
        end,
    {atomic, [X]} = mnesia:transaction(F),
    X.

get_guild_id(Name) ->
    F = fun() ->
                MatchHead = #guild{name = '$1',
                                   id= '$2',
                                   _ = '_'},
                Guards = [{'==', '$1', Name}],
                Result = '$2',
                mnesia:select(guild, [{MatchHead,
                                       Guards,
                                       [Result]
                                      }])
        end,
    {atomic, X} = mnesia:transaction(F),
    case X of
        [] ->
            nil;
        [Something] ->
            Something
    end.

get_guild_master(GuildId) ->
    #guild{master_id=Master} = get_guild(GuildId),
    Master.

get_guild_members(GuildID) ->
    #guild{members = Chars} = get_guild(GuildID),
    [get_char(ID) || ID <- Chars].

add_char_to_guild(GuildID, CharacterID) ->
    Guild = get_guild(GuildID),
    OldMembers = Guild#guild.members,
    NewGuild = Guild#guild{members=[CharacterID | OldMembers]},
    Fun = fun() ->
                  mnesia:write(NewGuild)
          end,
    mnesia:transaction(Fun).

give_world_item(Map, ItemID, Amount, X, Y) ->
    WI = get_world_items(Map),
    case WI of
        [] ->
            InitWI = #map_items{map_name=Map,
                                items=[#world_item{obj_id=1,
                                                   x=X,
                                                   y=Y,
                                                   item=ItemID,
                                                   slot=0,
                                                   amount=Amount}]},
            Fun = fun() ->
                          mnesia:write(InitWI)
                  end,
            mnesia:transaction(Fun),
            1;
        OldItems ->
            ObjID = length(OldItems)+1,
            NewItem = #world_item{obj_id=ObjID,
                                  x=X,
                                  y=Y,
                                  item=ItemID,
                                  slot=0,
                                  amount=Amount},
            NewItems = [NewItem|OldItems],
            NewWI = #map_items{map_name=Map,
                               items=NewItems},
            Fun = fun() ->
                          mnesia:write(NewWI)
                  end,
            mnesia:transaction(Fun),
            ObjID
    end.

get_world_items(Map) ->
    F = fun() ->
                MatchHead = #map_items{map_name = '$1',
                                       items = '$2'},
                Guards = [{'==', '$1', Map}],
                Result = '$2',
                mnesia:select(map_items, [{MatchHead,
                                           Guards,
                                           [Result]
                                          }])
        end,
    {atomic, X} = mnesia:transaction(F),
    case X of
        [] -> [];
        [Ls] -> Ls
    end.

get_world_item(Map, ObjectID) ->
    Items = get_world_items(Map),
    case lists:keysearch(ObjectID, #world_item.obj_id, Items) of
        false ->
            nil;
        {value, WorldItem} ->
            WorldItem
    end.

remove_world_item(Map, ObjectID) ->
    Items = get_world_items(Map),
    NewItems = lists:keydelete(ObjectID, #world_item.obj_id, Items),
    NewMapItems = #map_items{map_name=Map, items=NewItems},
    Fun = fun() ->
                  mnesia:write(NewMapItems)
          end,
    mnesia:transaction(Fun).

give_player_item(CharacterID, ItemID, Amount, Type) ->
    X = get_player_items(CharacterID),
    case X of
        [] ->
            Slot = 1,
            WI = #world_item{slot = Slot,
                             item = ItemID,
                             amount = Amount,
                             type = Type},
            InitInventory = #inventory{charid=CharacterID,
                                       items=[WI]},
            Fun = fun() ->
                          mnesia:write(InitInventory)
                  end,
            mnesia:transaction(Fun),
            Slot;
        [#inventory{items=Items}=Inv] ->
            Slot = length(Items)+1,
            WI = #world_item{slot = Slot,
                             item = ItemID,
                             amount = Amount,
                             type = Type},
            NewItems = Items ++ [WI],
            Inv1 = Inv#inventory{items=NewItems},
            Fun = fun() ->
                          mnesia:write(Inv1)
                  end,
            mnesia:transaction(Fun),
            Slot
    end.

get_player_items(CharacterID) ->
    F = fun() ->
                MatchHead = #inventory{charid = '$1',
                                       _ = '_'},
                Guards = [{'==', '$1', CharacterID}],
                Result = '$_',
                mnesia:select(inventory, [{MatchHead,
                                           Guards,
                                           [Result]
                                          }])
        end,
    {atomic, X} = mnesia:transaction(F),
    X.

get_player_item(CharacterID, Slot) ->
    X = get_player_items(CharacterID),
    case X of
        [] ->
            nil;
        [#inventory{items=Items}] ->
            case length(Items) < Slot of
                true ->
                    nil;
                false ->
                    lists:nth(Slot, Items)
            end
    end.

remove_player_item(CharacterID, Slot) ->
    [#inventory{items=Items}=InitInv] = get_player_items(CharacterID),
    NewItems = lists:keydelete(Slot, #world_item.slot, Items),
    NewInv = InitInv#inventory{items=NewItems},
    Fun = fun() ->
                  mnesia:write(NewInv)
          end,
    mnesia:transaction(Fun).

get_mob_data(MobID) ->
    F = fun() ->
                MatchHead = #mob_data{id = '$1',
                                      _ = '_'},
                Guards = [{'==', '$1', MobID}],
                Result = '$_',
                mnesia:select(mob_data, [{MatchHead,
                                          Guards,
                                          [Result]
                                         }])
        end,
    {atomic, X} = mnesia:transaction(F),
    case X of
        [] ->
            nil;
        [MobData] ->
            MobData
    end.

get_item_data(ItemID) ->
    F = fun() ->
                MatchHead = #item_data{id = '$1',
                                       _ = '_'},
                Guards = [{'==', '$1', ItemID}],
                Result = '$_',
                mnesia:select(item_data, [{MatchHead,
                                          Guards,
                                          [Result]
                                         }])
        end,
    {atomic, X} = mnesia:transaction(F),
    case X of
        [] ->
            nil;
        [ItemData] ->
            ItemData
    end.
