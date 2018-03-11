-module(monster_worker).

-behaviour(gen_server).

-export([ init/1
        , terminate/2
        , handle_info/2
        , handle_cast/2
        , handle_call/3
        , format_status/2
        , code_change/3
        , start_link/1 ]).

-record(monster_state, {hp=1000}).

start_link(Params) ->
    gen_server:start_link(?MODULE, [Params], []).

init([Hp]) ->
    State = #monster_state{hp=Hp},
    {ok, State}.

terminate(_, _State) ->
    ok.

handle_info(_, State) ->
    {noreply, State}.

handle_call({dec_hp, Dmg}, _From, #monster_state{hp = Hp} = State) ->
    NewHp = Hp - Dmg,
    {reply, {ok, NewHp}, State#monster_state{hp = NewHp}}.

handle_cast(stop, State) ->
    {stop, normal, State}.

format_status(_Opt, _Whatever) ->
    todo.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
