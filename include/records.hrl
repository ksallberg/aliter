%%% Global
-record(ids,
        {key,
         id}).

-record(map,
        {id,
         name,
         width,
         height}).

-record(npc,
        {id,
         name,
         sprite,
         objecttype = 0,
         map,
         coordinates,
         direction,
         main}).

-record(login_state,
        {server,
         tcp,
         db,
         die,
         account,
         id_a,
         id_b,
         packet_ver}).

-record(char_state,
        {server,
         tcp,
         db,
         die,
         char,
         account,
         id_a,
         id_b,
         packet_ver,
         rename,
         login_worker}).

-record(zone_state,
        {server,
         map,
         map_server,
         states = [],
         tcp,
         db,
         char,
         account,
         id_a,
         id_b,
         packet_ver,
         walk_timer,
         walk_prev,
         walk_path,
         walk_changed,
         npc,
         char_worker,
         is_walking=false
        }).

-record(map_state,
        {map,
         players = [],
         npcs = [],
         mobs = []}).

-record(
   nb_state,
   { port,
     packet_handler,
     worker_module,
     worker_args,
     server
   }
  ).

%%% Login tables
-record(account,
        {id,
         login,
         password,
         email = "",
         gender = 0,
         login_count = 0,
         last_login = 0,
         last_ip = "",
         gm_level = 0}).

%%% Character tables
-record(char,
        {id,
         num = 0,
         name = "",
         job = 0,
         base_level = 1,
         base_exp = 0,
         job_level = 1,
         job_exp = 0,
         zeny = 0,
         str = 1,
         agi = 1,
         vit = 1,
         int = 1,
         dex = 1,
         luk = 1,
         max_hp = 42,
         hp = 42,
         max_sp = 11,
         sp = 11,
         status_points = 0,
         skill_points = 0,
         hair_style = 0,
         hair_colour = 0,
         clothes_colour = 0,
         view_weapon = 0,
         view_shield = 0,
         view_head_top = 0,
         view_head_middle = 0,
         view_head_bottom = 0,
         map = "new_1-1",
         x = 53,
         y = 111,
         save_map = "new_1-1",
         save_x = 53,
         save_y = 111,
         online = 0,
         effects = 0,
         karma = 0,
         manner = 0,
         fame = 0,
         guild_position = 0,
         guild_taxed = 0,
         renamed = 0,
         account_id = 0,
         party_id = 0,
         guild_id = 0,
         pet_id = 0,
         homunculus_id = 0,
         mercenary_id = 0}).

%%% Zone server
%% Tables
-record(item,
        {id,
         name = "",
         clean_name = "",
         type = 0,
         price_buy = 0,
         price_sell = -1,
         weight = 0,
         attack = 0,
         defence = 0,
         range = 0,
         slots = 0,
         equip_jobs = 0,
         equip_upper = 0,
         equip_genders = 0,
         equip_locations = 0,
         weapon_level = 0,
         equip_level = 0,
         refineable = 0,
         view = 0,
         script = "",
         equip_script = "",
         unequip_script = ""}).

-record(monster,
        {id,
         name,
         translated_name,
         international_name,
         level = 1,
         hp = 1,
         sp = 1,
         base_exp = 0,
         job_exp = 0,
         attack_min = 0,
         attack_max = 0,
         def = 0,
         magic_def = 0,
         str = 1,
         agi = 1,
         vit = 1,
         int = 1,
         dex = 1,
         luk = 1,
         attack_type = 0,
         skill_range = 0,
         sight_range = 0,
         scale = 0,
         race = 0,
         element = 0,
         mode = 0,
         speed = 0,
         attack_delay = 0,
         attack_animation = 0,
         drop_items = [],
         drop_card = 0,
         mvp_exp = 0,
         mvp_bonus = 0,
         mvp_drops = []}).

-record(guild,
        {id,
         name = "",
         level = 1,
         capacity = 1,
         exp = 0,
         next_exp = 0,
         skill_points = 0,
         message_title = "",
         message_body = "",
         emblem = <<>>,
         castles = [],
         expulsions = [],
         positions = [],
         relationships = [],
         master_id = 0}).

%% Data
-record(guild_castle,
        {id,
         economy = 0,
         defence = 0,
         invested_economy,
         invested_defence,
         next_time = 0,
         pay_time = 0,
         created_time = 0,
         kafra_hired = 0,
         guardian_a = 0,
         guardian_b = 0,
         guardian_c = 0,
         guardian_d = 0,
         guardian_e = 0,
         guardian_f = 0,
         guardian_g = 0,
         guardian_h = 0}).

-record(guild_expulsion,
        {account_id,
         character_id,
         name = "",
         reason = ""}).

-record(guild_position,
        {id,
         name = "",
         mode = 0,
         exp_mode = 0}).

-record(guild_relationship,
        {a_id,
         b_id,
         type}).

-record(world_item, {slot, item, amount}).

%%% API Data
-record(apiresponse,
        {msg,
         info,
         account,
         char}).

-record(apirequest,
        {key,
         node,
         add_account,
         get_account,
         delete_account,
         update_account,
         add_char,
         get_char,
         delete_char,
         update_char}).
