-module(char_srv).
-behaviour(gen_server_tcp).

-include("records.hrl").
-include("ro.hrl").

-export([ start_link/1 ]).

-export([ init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3 ]).

-record(state, {db, sessions}).

start_link(_Conf) ->
    gen_server_tcp:start_link({local, char_server}, ?MODULE, ?CHAR_PORT, []).

init(Port) ->
    {ok, DB} = redis:connect(), % TODO: config

    {ok, _Keepalive} =
        timer:apply_interval(timer:seconds(30), redis, ping, [DB]),

    {ok,
     {Port, char_fsm, char_packets_24},
     {#state{db = DB, sessions = []}, [DB]}
    }.

handle_call(
  {verify_session, AccountID, CharacterID, SessionIDa},
  _From,
  State = #state{sessions = Sessions}) ->
    lager:log(info, self(), "Verifying session. ~p ~p ~p ~p",
              [{account, AccountID},
               {character, CharacterID},
               {id, SessionIDa},
               {sessions, Sessions}]),
    case proplists:lookup(AccountID, Sessions) of
        {AccountID, FSM, SessionIDa, _SessionIDb} ->
            {reply, {ok, FSM}, State};
        _ ->
            {reply, invalid, State}
    end;

handle_call({get_session, AccountID}, _From, State = #state{sessions = Sessions}) ->
    {reply, proplists:lookup(AccountID, Sessions), State};

handle_call(Request, _From, State) ->
    {reply, {illegal_request, Request}, State}.

handle_cast({add_session, Session}, State = #state{sessions = Sessions}) ->
    {noreply, State#state{sessions = [Session | Sessions]}};

handle_cast({remove_session, AccountID}, State = #state{sessions = Sessions}) ->
    {noreply, State#state{sessions = lists:keydelete(AccountID, 1, Sessions)}};

handle_cast({save_char, C}, State = #state{db = DB, sessions = Sessions}) ->
    db:save_char(DB, C),
    case proplists:lookup(C#char.account_id, Sessions) of
        {_AccountID, FSM, _SessionIDa, _SessionIDb} ->
            ChF = fun(St) ->St#char_state{char = C} end,
            gen_fsm:send_all_state_event(FSM, {update_state, ChF});
        _ ->
            ok
    end,
    {noreply, State};

handle_cast(_Cast, State) ->
    {noreply, State}.

handle_info({'EXIT', From, Reason}, State) ->
    lager:log(error, self(), "Character server got EXIT signal. ~p ~p",
              [{from, From}, {reason, Reason}]),
    {stop, normal, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
