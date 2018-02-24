-module(monster_srv).

-behaviour(gen_server).

-export([ init/1
        , terminate/2
        , handle_info/2
        , handle_cast/2
        , handle_call/3
        , format_status/2
        , code_change/3
        , start_link/0 ]).

-record(monster_state, {counter=1000}).

start_link() ->
    lager:log(info, self(), "Starting monster server ~n", []),
    gen_server:start_link(?MODULE, [], []).

init(_) ->
    State = #monster_state{counter=1000},
    erlang:register('monster_srv', self()),
    {ok, State}.

terminate(_, _State) ->
    ok.

handle_info(_, State) ->
    {noreply, State}.

handle_call(next_id, _From, #monster_state{counter=C}) ->
    {reply, C+1, #monster_state{counter=C+1}}.

handle_cast(_, State) ->
    {noreply, State}.

format_status(_Opt, _Whatever) ->
    todo.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
