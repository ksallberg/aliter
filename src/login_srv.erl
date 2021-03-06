-module(login_srv).

-behaviour(gen_server).

-include("records.hrl").
-include("ro.hrl").

-export([start_link/0]).

-export([ init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3 ]).

start_link() ->
    Port = ?LOGIN_PORT,
    lager:log(info, self(), "Starting login server at port: ~p~n", [Port]),
    gen_server:start_link({local, login_server}, ?MODULE, Port, []).

init(Port) ->
    %% Start TCP listener, and worker
    {ok, _} = ranch:start_listener(login_listener, ranch_tcp,
                                   [{port, Port}], ragnarok_proto,
                                   [login_packets]),
    {ok, []}.

handle_call({verify_session, AccountID, LoginIDa, LoginIDb}, _From, Sessions) ->
    case proplists:lookup(AccountID, Sessions) of
        {AccountID, KeptState, LoginIDa, LoginIDb} ->
            {reply, {ok, KeptState}, Sessions};
        _ ->
            {reply, invalid, Sessions}
    end;
handle_call(Request, _From, Sessions) ->
    {reply, {illegal_request, Request}, Sessions}.

handle_cast({add_session, Session}, Sessions) ->
    lager:log(info, self(), "Login server adding session ~p",
              [{session, Session}]),
    {noreply, [Session | Sessions]};
handle_cast({remove_session, AccountID}, Sessions) ->
    lager:log(info, self(), "DELETE SESSION ~p",
              [Sessions]),
    {noreply, lists:keydelete(AccountID, 1, Sessions)};
handle_cast(_Cast, Sessions) ->
    {noreply, Sessions}.

handle_info({'EXIT', From, Reason}, Sessions) ->
    lager:log(error, "Login server got EXIT signal ~p ~p",
              [{from, From}, {reason, Reason}]),
    {stop, normal, Sessions};
handle_info(Info, Sessions) ->
    lager:log(info, self(), "Login server got info ~p", [{info, Info}]),
    {noreply, Sessions}.

terminate(_Reason, _Sessions) ->
    ok.

code_change(_OldVsn, Sessions, _Extra) ->
    {ok, Sessions}.
