-module(char_srv).

-behaviour(gen_server).

-include("records.hrl").
-include("ro.hrl").

-export([ start_link/0 ]).

-export([ init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3 ]).

-record(state, {sessions}).

start_link() ->
    Port = ?CHAR_PORT,
    gen_server:start_link({local, char_server}, ?MODULE, Port, []).

init(Port) ->
    {ok, _} = ranch:start_listener(char_listener, ranch_tcp, [{port, Port}],
                                   ragnarok_proto, [char_packets_24]),
    {ok, #state{sessions = []}}.

handle_call({verify_session, AccountID, CharacterID, SessionIDa},
            _From,
            State = #state{sessions = Sessions}) ->
    lager:log(info, self(), "Verifying session. ~p ~p ~p ~p",
              [{account, AccountID},
               {character, CharacterID},
               {id, SessionIDa},
               {sessions, Sessions}]),
    case proplists:lookup(AccountID, Sessions) of
        {AccountID, Worker, SessionIDa, _SessionIDb} ->
            {reply, {ok, Worker}, State};
        _ ->
            {reply, invalid, State}
    end;
handle_call({get_session, AccountID}, _From,
            State = #state{sessions = Sessions}) ->
    {reply, proplists:lookup(AccountID, Sessions), State};

handle_call(Request, _From, State) ->
    {reply, {illegal_request, Request}, State}.

handle_cast({add_session, Session}, State = #state{sessions = Sessions}) ->
    {noreply, State#state{sessions = [Session | Sessions]}};

handle_cast({remove_session, AccountID}, State = #state{sessions = Sessions}) ->
    {noreply, State#state{sessions = lists:keydelete(AccountID, 1, Sessions)}};

handle_cast({save_char, C}, State = #state{sessions = Sessions}) ->
    db:save_char(C),
    case proplists:lookup(C#char.account_id, Sessions) of
        {_AccountID, Worker, _SessionIDa, _SessionIDb} ->
            ChF = fun(St) ->St#char_state{char = C} end,
            gen_server:cast(Worker, {update_state, ChF});
        _ ->
            ok
    end,
    {noreply, State};

%% When exiting, we do not want to overwrite the guild_id
%% If it has been set without our knowing
%% FIXME: Should joining a guild really have updated our char_srv state?
handle_cast({save_char_exit, #char{id=CharID}=C},
            State = #state{sessions = Sessions}) ->
    #char{guild_id=GID} = db:get_char(CharID),
    NewChar = C#char{guild_id=GID},
    db:save_char(NewChar),
    case proplists:lookup(C#char.account_id, Sessions) of
        {_AccountID, Worker, _SessionIDa, _SessionIDb} ->
            ChF = fun(St) ->St#char_state{char = C} end,
            gen_server:cast(Worker, {update_state, ChF});
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
