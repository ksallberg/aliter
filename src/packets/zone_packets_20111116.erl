-module(zone_packets_20111116).

%% When length per element is given, such as in the case of equipment,
%% it defines length in terms of bytes. So, (bits / 8).

-include("records.hrl").
-include("ro.hrl").

-export([ unpack/1
        , pack/2
        , packet_size/1 ]).

-define(WALKSPEED, 150).

packet_size(X) ->
    packets:packet_size(X).

unpack(<<16#083c:16/little,
         AccountID:32/little,
         CharacterID:32/little,
         LoginIDa:32/little,
         _ClientTick:32,
         Gender:8>>) ->
    {connect, AccountID, CharacterID, LoginIDa, Gender};
%% sitting, standing:
unpack(<<16#08aa:16/little,
         Target:32/little,
         Action:8>>) ->
    {action_request, Target, Action};
%% CZ_NOTIFY_ACTORINIT
unpack(<<16#7d:16/little>>) ->
    map_loaded;
unpack(<<16#85:16/little, _:16, Head:16/little, _:24, Body:8>>) ->
    {change_direction, Head, Body};
unpack(<<16#016e:16/little, GID:32/little,
         Header:60/binary, Body:120/binary>>) ->
    {guild_msg_upd, GID, Header, Body};
unpack(<<16#0817:16/little, Tick:32/little>>) ->
    {tick, Tick};
unpack(<<16#0090:16/little, NPCID:32/little, _:8>>) ->
    {npc_activate, NPCID};
%% CZ_REQNAME
unpack(<<16#88a:16/little, ReqName:32/little>>) ->
    {request_name, ReqName};
unpack(<<16#99:16/little, Length:16/little, Message/binary>>)
  when byte_size(Message) == (Length - 4) ->
    {broadcast, string:strip(binary_to_list(Message), right, 0)};
unpack(<<100,3, Position:3/little-binary-unit:8>>) ->
    {walk, decode_position(Position)};
unpack(<<16#b2:16/little, Type:8>>) ->
    {char_select, Type};
unpack(<<16#b8:16/little, ActorID:32/little, Selection:8>>) ->
    {npc_menu_select, ActorID, Selection};
unpack(<<16#b9:16/little, ActorID:32/little>>) ->
    {npc_next, ActorID};
unpack(<<16#bf:16/little, EmoticonID:8>>) ->
    {emotion, EmoticonID};
unpack(<<16#c1:16/little>>) ->
    player_count;
unpack(<<16#0151:16/little, GuildID:32/little>>) ->
    %% request_guild_status;
    {guild_emblem, GuildID};
unpack(<<16#f3:16/little, Length:16/little, Message/little-binary-unit:8>>)
  when byte_size(Message) == (Length - 4) ->
    {speak, string:strip(binary_to_list(Message), right, 0)};
unpack(<<16#146:16/little, ActorID:32/little>>) ->
    {npc_close, ActorID};
unpack(<<16#14d:16/little>>) ->
    request_guild_status;
unpack(<<16#14f:16/little, Page:32/little>>) ->
    {request_guild_info, Page};
unpack(<<16#18a:16/little, _:16>>) ->
    quit;
unpack(<<16#21d:16/little, IsLess:32/little>>) ->
    {less_effect, IsLess};
unpack(<<16#0165:16/little,
         CharId:32/little,
         GuildName/binary>>) ->
    GName = string:strip(binary_to_list(GuildName), right, 0),
    {create_guild, CharId, GName};
unpack(<<16#0118:16/little>>) ->
    cease_attack;
unpack(<<16#02c4:16/little,
         SkillLvl:16/little,
         SkillID:16/little,
         Target:32/little>>) ->
    {use_skill, SkillLvl, SkillID, Target};
unpack(<<16#0a9:16/little,
         Index:16/little,
         Position:16/little>>) ->
    {wear_equip, Index, Position};
unpack(<<16#0ab:16/little,
         Index:16/little>>) ->
    {unequip, Index};
unpack(<<16#885:16/little,
         Index:16/little,
         Amount:16/little>>) ->
    {drop, Index, Amount};
unpack(<<16#815:16/little,
         ObjectID:32/little>>) ->
    {pick_up, ObjectID};
unpack(Unknown) ->
    lager:log(warning, self(), "hmm zone packets Got unknown data ~p",
              [Unknown]),
    unknown.

pack(accept, {Tick, {X, Y, D}}) ->
    <<16#73:16/little,
      Tick:32/little,
      (encode_position(X, Y, D)):3/binary,
      5,   % X size(?), static
      5>>; % Y size(?), static
pack(show_npc, N) -> % TODO: This isn't actually specific to NPCs
    {X, Y} = N#npc.coordinates,
    D = case N#npc.direction of
            north -> 0;
            northwest -> 1;
            west -> 2;
            southwest -> 3;
            south -> 4;
            southeast -> 5;
            east -> 6;
            northeast -> 7
        end,
    <<16#78:16/little,
      (N#npc.objecttype):8, % Object type
      (N#npc.id):32/little,
      ?WALKSPEED:16/little,
      0:3/unit:16, %% Body, Health, Effect state
      (N#npc.sprite):16/little, % Job/NPC sprite
      0:9/unit:16, %% Head, Weapon, Accessory, Shield,
                   %% Accessory2, Accessory3,
                   %% Head Palette, Body Palette, Head Dir
      0:32, % GUID
      0:3/unit:16, % GEmblemVer, Honor, Virtue
      0:8, % Is PK mode on?
      0:8, % Sex
      (encode_position(X, Y, D)):3/binary,
      0:3/unit:8, % X size, Y size, State
      1:16/little>>; % Level (e.g. for aura)
pack(tick, Tick) ->
    <<16#7f:16/little, Tick:32/little>>;
pack(vanish, {ActorID, Type}) ->
    <<16#80:16/little, ActorID:32/little, Type:8>>;
pack(actor_move, {ActorID, {FromX, FromY}, {ToX, ToY}, Tick}) ->
    <<16#86:16/little,
      ActorID:32/little,
      (encode_move(FromX, FromY, ToX, ToY)):6/little-binary-unit:8,
      Tick:32/little>>;
pack(move, {{FromX, FromY}, {ToX, ToY}, Tick}) ->
    <<16#87:16/little,
      Tick:32/little,
      (encode_move(FromX, FromY, ToX, ToY)):6/little-binary-unit:8>>;
pack(actor_message, {ActorID, Message}) ->
    [<<16#8d:16/little,
       (length(Message) + 9):16/little,
       ActorID:32/little>>,
     list_to_binary(Message),
     <<0>>];
pack(message, Message) ->
    X = iolist_to_binary(Message),
    [<<16#8e:16/little, (byte_size(X) + 5):16/little>>,
     X,
     <<0>>];
pack(warp_map, {Map, X, Y}) ->
    [<<16#91:16/little>>,
     pad_to([Map, <<".gat">>], 16),
     <<X:16/little,
       Y:16/little>>];
pack(warp_zone, {Map, X, Y, {ZA, ZB, ZC, ZD}, Port}) ->
    [<<16#92:16/little>>,
     pad_to([Map, <<".gat">>], 16),
     <<X:16/little,
       Y:16/little,
       ZA:8,
       ZB:8,
       ZC:8,
       ZD:8,
       Port:16/little>>];
pack(actor_name, {ActorID, Name}) ->
    [<<16#95:16/little, ActorID:32/little>>,
     pad_to(Name, 24)];
pack(broadcast, Message) ->
    [<<16#9a:16/little, (length(Message) + 5):16/little>>,
     list_to_binary(Message),
     <<0>>];
pack(change_direction, {ActorID, HeadDir, BodyDir}) ->
    <<16#9c:16/little, ActorID:32/little, HeadDir:16/little, BodyDir:8>>;
pack(param_change, {Type, Value}) ->
    <<16#b0:16/little, Type:16/little, Value:32/little>>;
pack(param_change_long, {Type, Value}) ->
    <<16#b1:16/little, Type:16/little, Value:32/little>>;
pack(confirm_back_to_char, {Type}) ->
    <<16#b3:16/little, Type:8>>;
pack(dialog, {ActorID, Message}) ->
    [<<16#b4:16/little,
       (length(Message) + 9):16/little,
       ActorID:32/little>>,
     list_to_binary(Message),
     <<0>>];
pack(dialog_next, ActorID) ->
    <<16#b5:16/little, ActorID:32/little>>;
pack(dialog_close, ActorID) ->
    <<16#b6:16/little, ActorID:32/little>>;
pack(dialog_menu, {ActorID, Choices}) ->
    Menu = string:join(Choices, ":"),
    [ <<16#b7:16/little,
        (length(Menu) + 8):16/little,
        ActorID:32/little>>,
      list_to_binary(Menu)
    ];
pack(status, C) ->
    <<16#bd:16/little,
      (C#char.status_points):16/little,
      (C#char.str):8/little,
      0:8/little,
      (C#char.agi):8/little,
      0:8/little,
      (C#char.vit):8/little,
      0:8/little,
      (C#char.int):8/little,
      0:8/little,
      (C#char.dex):8/little,
      0:8/little,
      (C#char.luk):8/little,
      0:8/little,
      7:16/little,
      0:16/little,
      6:16/little,
      5:16/little,
      0:16/little,
      5:16/little,
      0:16/little,
      5:16/little,
      6:16/little,
      6:16/little,
      1:16/little,
      2:16/little,
      0:8, % Nothing
      0:8, % Nothing
      0:8, % Nothing, v-
      0:8>>; % TODO
pack(emotion, {ActorID, EmoticonID}) ->
    <<16#c0:16/little, ActorID:32/little, EmoticonID:8>>;
pack(player_count, Count) ->
    <<16#c2:16/little, Count:32/little>>;
pack(skill_list, Skills) ->
    [ <<16#10f:16/little,
        (37 * length(Skills) + 4):16/little>>,
      [ [ <<ID:16/little,
            Type:16/little,
            0:16, % Nothing
            Level:16/little,
            Range:16/little,
            SP:16/little>>,
          pad_to(Name, 24),
          <<Up>>
        ] || {ID, Type, Level, SP, Range, Name, Up} <- Skills]];
pack(attack_range, Range) ->
    <<16#13a:16/little, Range:16/little>>;
pack(status_change, {Stat, Value, Bonus}) ->
    <<16#141:16/little, Stat:32/little, Value:32/little, Bonus:32/little>>;
% 043f
% <index>.W
% <id>.L
% <state>.B
% <remain msec>.L
% { <val>.L }*3
% (ZC_MSG_STATE_CHANGE2) [used exclusively for starting statuses on pcs]
pack(state_change, {Index, ID, State, Remain}) ->
    <<16#043f:16/little,
      Index:16/little,
      ID:32/little,
      State:8/little,
      Remain:32/little,
      0:32/little,
      0:32/little,
      0:32/little>>;
pack(guild_relationships, Relationships) ->
    [ <<16#014c:16/little,
        (32 * length(Relationships) + 4):16/little>>,
      [ [ <<(R#guild_relationship.type):32/little,
            (R#guild_relationship.b_id):32/little>>,
                                                % TODO
          pad_to("Guild name here!", 24)
        ] || R <- Relationships
      ]
    ];
pack(guild_status, State) ->
    case State of
        master ->
            <<16#014e:16/little, 16#d7:32/little>>;
        _Other ->
            <<16#014e:16/little, 16#57:32/little>>
    end;
pack(guild_message, Message) ->
    [ <<16#17f:16/little, (length(Message) + 5):16/little>>,
      list_to_binary(Message),
      <<5>>
    ];
pack(quit_response, QuitResponse) ->
    <<16#18b:16/little, QuitResponse:16/little>>;
pack(actor_name_full, {AccountID, Name, Party, Guild, Position}) ->
    [<<16#195:16/little, AccountID:32/little>>,
     pad_to(Name, 24),
     pad_to(Party, 24),
     pad_to(Guild, 24),
     pad_to(Position, 24)];
pack(update_gd_id, Guild) ->
    [<<16#016c:16/little,
       (Guild#guild.id):32/little,
       0:32/little, %% emblem id
       0:32/little, %% mode
       1:8, %% isMaster
       0:32/little>>, %% inter sid
       pad_to(Guild#guild.name, 24)];
% 01b6 <guild id>.L <level>.L <member num>.L
% <member max>.L <exp>.L <max exp>.L <points>.L
% <honor>.L <virtue>.L <emblem id>.L <name>.24B
% <master name>.24B <manage land>.16B <zeny>.L (ZC_GUILD_INFO2)
pack(guild_info, Guild) ->
    [<<16#01B6:16/little,
       (Guild#guild.id):32/little,
       (Guild#guild.level):32/little,
       0:32/little, % TODO: Online count
       (16 * Guild#guild.level):32/little, % TODO: Verify this
       11:32/little, % TODO: Average level
       (Guild#guild.exp):32/little,
       (Guild#guild.next_exp):32/little,
       0:32/little, % TODO: Tax points
       0:32/little, % TODO: Tendency Left/Right
       0:32/little, % TODO: Tendency Down/Up
       0:32/little>>, % TODO: Emblem ID
     pad_to(Guild#guild.name, 24),
     pad_to(Guild#guild.master_name, 24),
     pad_to("Hmmm", 16),
    <<0:32/little>>]; %% zeny

%% Guild member manager information (ZC_MEMBERMGR_INFO).
%% 0154 <packet len>.W { <account>.L
%%                       <char id>.L <hair style>.W <hair color>.W
%%                       <gender>.W <class>.W <level>.W <contrib exp>.L
%%                       <state>.L <position>.L <memo>.50B <name>.24B }*
%% state:
%%     0 = offline
%%     1 = online
%% memo:
%%     probably member's self-introduction
%%       (unused, no client UI/packets for editing it)

pack(guild_members, Mems) ->
    [<<16#154:16/little,
       (104 * length(Mems) + 4):16/little>>,
       [[<<(Mem#char.account_id):32/little,
          (Mem#char.id):32/little,
          (Mem#char.hair_style):16/little,
          (Mem#char.hair_colour):16/little,
          0:16/little, %% gender
          (Mem#char.job):16/little,
          (Mem#char.base_level):16/little,
          100:32/little, %% exp contrib
          1:32/little, %% state: online or offline
          0:32/little>>, %% position
          pad_to("text", 50),
          pad_to(Mem#char.name, 24)]
        || Mem <- Mems]];

pack(change_look, Character) ->
    <<16#1d7:16/little,
      (Character#char.account_id):32/little,
      2:8, % Type
      (Character#char.view_weapon):16/little,
      (Character#char.view_shield):16/little>>;
pack(actor, {State, A, C}) ->
    Header =
        case State of
            new -> 16#22b;
            _ -> 16#22a
        end,
    [<<Header:16/little,
       (A#account.id):32/little,
       ?WALKSPEED:16/little, % TODO: Walk speed
       0:16/little, % TODO: Status
       0:16/little, % TODO: Ailments
       0:16/little, % TODO: Option
       0:16, % Nothing
       (C#char.job):16/little,
       (C#char.hair_style):16/little,
       (C#char.view_weapon):16/little,
       (C#char.view_shield):16/little,
       (C#char.view_head_bottom):16/little,
       (C#char.view_head_top):16/little,
       (C#char.view_head_middle):16/little,
       (C#char.hair_colour):16/little,
       (C#char.clothes_colour):16/little,
       0:16/little, % TODO: Head direction (test this)
       (C#char.guild_id):32/little,
       (C#char.guild_id):16/little, % Guild emblem ID
       (C#char.manner):16/little, % Manners
       0:16/little, % Effect
       0:16, % Nothing,
       (C#char.karma):8, % Karma
       (A#account.gender):8, % Gender
       (encode_position(C#char.x, C#char.y, 0)):3/binary-unit:8,
       5:8,
       5:8>>,
      case State of
          new ->
              <<(C#char.base_level):16/little>>;
          _ ->
              <<0:8, % Nothing
                (C#char.base_level):16/little>>
      end];
pack(walking_actor, {A, C, Tick}) -> % TODO: Incomplete
    <<16#22c:16/little,
      0:8, % Nothing
      (A#account.id):32/little,
      ?WALKSPEED:16/little, % TODO: Walk speed
      0:16/little, % TODO: Status
      0:16/little, % TODO: Ailments
      0:16/little, % TODO: Option
      0:16, % Nothing
      (C#char.job):16/little,
      (C#char.hair_style):16/little,
      (C#char.view_weapon):16/little,
      (C#char.view_shield):16/little,
      (C#char.view_head_bottom):16/little,
      Tick:32/little,
      (C#char.view_head_top):16/little,
      (C#char.view_head_middle):16/little,
      (C#char.hair_colour):16/little,
      (C#char.clothes_colour):16/little,
      0:16/little, % TODO: Head direction (test this)
      (C#char.guild_id):32/little,
      (C#char.guild_id):16/little, % Guild emblem ID
      (C#char.manner):16/little, % Manners
      0:16/little, % Effect
      0:16, % Nothing,
      (C#char.karma):8, % Karma
      (A#account.gender):8, % Gender
      (encode_position(C#char.x, C#char.y, 0)):3/binary,
      5:8,
      5:8,
      0:8/unit:3, % Nothing
      (C#char.base_level):16/little>>;
pack(account_id, AccountID) ->
    <<16#283:16/little, AccountID:32/little>>;
pack(hotkeys, _Hotkeys) ->
    [<<16#2b9:16/little>>,
     list_to_binary(lists:duplicate(189, 0))];
pack(party_invite_state, State) ->
    <<16#2c9:16/little, State:8>>;
pack(equipment, EquipmentLs) ->
    MapF = fun(#equip{index = Index,
                      id = ID,
                      type = Type,
                      identified = Identified,
                      location = Location,
                      wearstate = WearState,
                      is_damaged = IsDamaged,
                      refining_level = RefiningLevel,
                      card1 = Card1,
                      card2 = Card2,
                      card3 = Card3,
                      card4 = Card4,
                      hire_expire_date = HireExpireDate,
                      bind_on_equip_type = BindOnEquipType,
                      sprite_number = SpriteNumber}) ->
                   <<Index:16/little,
                     ID:16/little,
                     Type:8,
                     Identified:8,
                     Location:16/little,
                     WearState:16/little,
                     IsDamaged:8/little,
                     RefiningLevel:8/little,
                     Card1:16/little,
                     Card2:16/little,
                     Card3:16/little,
                     Card4:16/little,
                     HireExpireDate:32/little,
                     BindOnEquipType:16/little,
                     SpriteNumber:16/little>>
           end,
    L  = (16 + 16 + 8 + 8 + 16 + 16 + 8 + 8
          + 16 + 16 + 16 + 16 + 32 + 16 + 16) div 8,
    Res = [<<16#2d0:16/little,
             (L * length(EquipmentLs) + 4):16/little>>,
           lists:map(MapF, EquipmentLs)],
    Res;



%% inventorylistequipType = 0x2d0,

%% 16+16+8+8+16+16+8+8+32+16+16+
%% int16 index;
%% uint16 ITID;
%% uint8 type;
%% uint8 IsIdentified;
%% uint16 location;
%% uint16 WearState;
%% uint8 IsDamaged;
%% uint8 RefiningLevel;
%% struct EQUIPSLOTINFO slot;
%% int32 HireExpireDate;
%% uint16 bindOnEquipType;
%% uint16 wItemSpriteNumber;

pack(inventory_equip, Inventory) ->
    [<<16#2d0:16/little,
       (28 * length(Inventory) + 4):16/little>>,
     [<<(I#world_item.slot):16/little,
        (I#world_item.item):16/little,
        (I#world_item.type):8,
        1:8, % identified
        (get_equip_for(I#world_item.item)):16/little, % location
        0:16/little, % WearState(?)
        0:8/little, % isdamaged
        0:8/little, % refining level

        0:16/little, % TODO: card 1
        0:16/little, % TODO: card 2
        0:16/little, % TODO: card 3
        0:16/little, % TODO: card 4

        0:32/little, % hireexpiredate
        0:16/little, % bindonequiptype
        0:16/little>> % wItemSpriteNumber
          || I <- Inventory]];

pack(inventory, Inventory) ->
    [<<16#2e8:16/little,
       (22 * length(Inventory) + 4):16/little>>,
     [<<(I#world_item.slot):16/little,
        (I#world_item.item):16/little,
        (I#world_item.type):8,
        1:8, % TODO: identified
        (I#world_item.amount):16/little,
        0:16/little, % TODO: WearState(?)
        0:16/little, % TODO: card 1
        0:16/little, % TODO: card 2
        0:16/little, % TODO: card 3
        0:16/little, % TODO: card 4
        0:32/little>> % expiration
          || I <- Inventory]];
pack(give_item,
     {Index,
      Amount,
      ID,
      Identified,
      Damaged,
      Refine,
      Card1,
      Card2,
      Card3,
      Card4,
      EquipLocation,
      Type, % (3 = etc, 4 = weapon)
      Result,
      ExpireTime,
      BindOnEquipType
     }) ->
    <<16#2d4:16/little,
      Index:16/little,
      Amount:16/little,
      ID:16/little, % item ID num?
      Identified:8,
      Damaged:8,
      Refine:8,
      Card1:16/little,
      Card2:16/little,
      Card3:16/little,
      Card4:16/little,
      EquipLocation:16/little,
      Type:8,
      Result:8,
      ExpireTime:32/little,
      BindOnEquipType:16/little>>;
pack(view_equip_state, State) ->
    <<16#2da:16/little, State:8>>;
pack(actor_effect,
     {AID, BID, Tick, ASpeed, BSpeed, Damage, Div, Type, Damage2}) ->
    <<16#8a:16/little,
      AID:32/little,
      BID:32/little,
      Tick:32/little,
      ASpeed:32/little,
      BSpeed:32/little,
      Damage:16/little,
      Div:16/little,
      Type:8,
      Damage2:16/little>>;
pack(drop_item, {Index, Amount}) ->
    <<16#af:16/little,
      Index:16/little,
      Amount:16/little>>;
pack(item_disappear, ObjectID) ->
    <<16#a1:16/little,
      ObjectID:32/little>>;
pack(item_on_ground,
     {ObjectID, ItemID, Identified, X, Y, SubX, SubY, Amount}) ->
    <<16#9e:16/little,
      ObjectID:32/little,
      ItemID:16/little,
      Identified:8,
      X:16/little,
      Y:16/little,
      SubX:8,
      SubY:8,
      Amount:16/little>>;
pack(sprite,
     {CharID, Type, Value}) ->
    <<16#01d7:16/little,
      CharID:32/little,
      Type:8/little,
      Value:32/little>>;
pack(monster, {SpriteID, X, Y, GID}) ->
    <<X2, Y2, D>> = encode_position(X, Y, 0),
    <<16#07c:16/little, %% PacketType
      5:8/little, %% objecttype
      GID:32/little, %% GID
      0:16/little, %% speed
      0:16/little, %% bodyState
      0:16/little, %% healthState
      0:16/little, %% effectState
      0:16/little, %% head
      0:16/little, %% weapon
      0:16/little, %% accessory
      SpriteID:16/little, %% job
      0:16/little, %% shield
      0:16/little, %% accessory2
      0:16/little, %% accessory3
      0:16/little, %% headpalette
      0:16/little, %% bodypalette
      0:16/little, %% headDir
      0:8/little, %% isPKModeON
      0:8/little, %% sex
      X2:8, Y2:8, D:8,
      0:8/little, %% xSize
      0:8/little>>; %% ySize
pack(attack, {Caller, Callee, Time, SrcSpeed, DstSpeed, %% ZC_NOTIFY_ACT2
              Damage, IsSPDamage, Div, Damage2, Type}) ->
    <<16#08c8:16/little,
      Caller:32/little,
      Callee:32/little,
      Time:32/little,
      SrcSpeed:32/little,
      DstSpeed:32/little,
      Damage:32/little,
      IsSPDamage:8,
      Div:16/little,
      Type:8,
      Damage2:32/little>>;
% 01de
% <skill id>.W
% <src id>.L
% <dst id>.L
% <tick>.L
% <src delay>.L
% <dst delay>.L
% <damage>.L
% <level>.W
% <div>.W
% <type>.B (ZC_NOTIFY_SKILL2)
pack(notify_skill, {SkillID, CasterID, TargetID, Tick,
                    SrcDelay, TargetDelay, Dmg, Level, Div, Type}) ->
    <<16#01de:16/little,
      SkillID:16/little,
      CasterID:32/little,
      TargetID:32/little,
      Tick:32/little,
      SrcDelay:32/little,
      TargetDelay:32/little,
      Dmg:32/little,
      Level:16/little,
      Div:16/little,
      Type:8/little>>;
pack(use_skill, {SKID, Level, TargetID, SourceID, Result}) ->
    <<16#011a:16/little,
      SKID:16/little,
      Level:16/little,
      TargetID:32/little,
      SourceID:32/little,
      Result:8/little>>;
pack(guild_msg, {Header, Body}) ->
    [<<16#016f:16/little>>,
       pad_to(Header, 60),
       pad_to(Body, 120)];
%% /// 00ac <index>.W <equip location>.W <result>.B
pack(takeoff, {Index, Position}) ->
    [<<16#00ac:16/little,
       Index:16/little,
       Position:16/little,
       1:8>>];
pack(Header, Data) ->
    lager:log(error, self(), "Cannot pack unknown data. ~p ~p",
              [Header, Data]),
    <<>>.

decode_position(<<XNum, YNum, DNum>>) ->
    X = (XNum bsl 2) bor ((YNum band 16#c0) bsr 6),
    Y = ((YNum band 16#3F) bsl 4) bor ((DNum band 16#f0) bsr 4),
    D = DNum band 16#0F,
    {X, Y, D}.

encode_position(X, Y, D) ->
    A = (X bsr 2) band 16#ff,
    B = ((X bsl 6) bor ((Y bsr 4) band 16#3f)) band 16#ff,
    C = ((Y bsl 4) bor (D band 16#0f)) band 16#ff,
    <<A, B, C>>.

encode_move(X, Y, ToX, ToY) ->
    A = (X bsr 2) band 16#ff,
    B = ((X bsl 6) bor ((Y bsr 4) band 16#3f)) band 16#ff,
    C = ((Y bsl 4) bor ((ToX bsr 6) band 16#0f)) band 16#ff,
    D = ((ToX bsl 2) bor ((ToY bsr 8) band 16#03)) band 16#ff,
    E = ToY band 16#ff,
    F = ((8 bsl 4) bor (8 band 16#0f)) band 16#ff,
    <<A, B, C, D, E, F>>.

pad_to(Bin, Size) ->
    Binary = iolist_to_binary(Bin),
    [ binary:part(Binary, 0, min(byte_size(Binary), Size))
    , binary:copy(<<0>>, Size - byte_size(Binary))].

get_equip_for(ItemID) ->
    ItemData = db:get_item_data(ItemID),
    case ItemData of
        nil ->
            0;
        #item_data{equip_locations=EquipLocation} ->
            EquipLocation
    end.
