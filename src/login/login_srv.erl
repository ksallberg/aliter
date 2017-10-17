-module(login_srv).

-behaviour(gen_server_tcp).

-include("include/records.hrl").
-include("include/ro.hrl").

-export([start_link/0]).

-export([ init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3 ]).

start_link() ->
    Port = ?LOGIN_PORT,
    io:format("login_srv... start_link~n"),
    lager:log(info, self(), "Starting login server at port: ~p~n", [Port]),
    gen_server_tcp:start_link({local, login_server}, ?MODULE, Port, []).

init(Port) ->
    io:format("login_srv... init~n"),
    {ok, DB} = redis:connect(),
    {ok, _Keepalive} =
        timer:apply_interval(timer:seconds(30), redis, ping, [DB]),
    io:format("login_srv... init2~n"),
    {ok, {Port, login_fsm, login_packets}, {[], [DB]}}.

handle_call({verify_session, AccountID, LoginIDa, LoginIDb}, _From, Sessions) ->
  case proplists:lookup(AccountID, Sessions) of
    {AccountID, FSM, LoginIDa, LoginIDb} ->
      {reply, {ok, FSM}, Sessions};
    _ ->
      {reply, invalid, Sessions}
  end;
handle_call({get_session, AccountID}, _From, Sessions) ->
  {reply, proplists:lookup(AccountID, Sessions), Sessions};
handle_call(Request, _From, Sessions) ->
  log:debug("Login server got call.", [{call, Request}]),
  {reply, {illegal_request, Request}, Sessions}.

handle_cast({add_session, Session}, Sessions) ->
  log:debug("Login server adding session.", [{session, Session}]),
  {noreply, [Session | Sessions]};
handle_cast({remove_session, AccountID}, Sessions) ->
  log:debug("Login server removing session.", [{account, AccountID}]),
  {noreply, lists:keydelete(AccountID, 1, Sessions)};
handle_cast(Cast, Sessions) ->
  log:debug("Login server got cast.", [{cast, Cast}]),
  {noreply, Sessions}.

handle_info({'EXIT', From, Reason}, Sessions) ->
  log:error("Login server got EXIT signal.", [{from, From}, {reason, Reason}]),
  {stop, normal, Sessions};
handle_info(Info, Sessions) ->
  log:debug("Login server got info.", [{info, Info}]),
  {noreply, Sessions}.

terminate(_Reason, _Sessions) ->
  log:info("Login server terminating.", [{node, node()}]),
  ok.

code_change(_OldVsn, Sessions, _Extra) ->
  {ok, Sessions}.
