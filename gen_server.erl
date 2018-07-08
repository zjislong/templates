%%%'   HEADER
%%% @author {{author_name}} <{{author_email}}>
%%% @copyright {{copyright_year}} {{author_name}}
%%% @doc gen_server callback module implementation:
%%% {{description}}
%%% @end
-module({{name}}_srv).
-author('{{author_name}} <{{author_email}}>').

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).
-export([code_change/3]).
-export([stop/0, terminate/2]).

% TODO: If unnamed server, remove definition below.
-define(SERVER, ?MODULE).
%%%.
%%%'   TYPE DEFINITIONS
-type start_link_error() :: {already_started, pid()} | term().

%%%.
%%%'   PUBLIC API

%% @doc starts gen_server implementation and caller links to the process too.
-spec start_link() -> {ok, pid()} | ignore | {error, start_link_error()}.
start_link() ->
  % TODO: decide whether to name gen_server callback implementation or not.
  % gen_server:start_link(?MODULE, [], []). % for unnamed gen_server
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc stops gen_server implementation process
-spec stop() -> ok.
stop() ->
  gen_server:cast(?SERVER, stop).

% TODO: add more public API here...

%%%.
%%%'   CALLBACKS
%% @callback gen_server
init(State) ->
  {ok, State}.

%% @callback gen_server
handle_call(Req, From, State) ->
    try
      do_handle_call(Req, From, State)
  catch
      C:R:Stacktrace ->
        io:format("{{name}} handle_call:~p fail:{~p,~p,~p}", [Req,C,R,Stacktrace]),
        {reply, C, State}
  end.

%% @callback gen_server
handle_cast(stop, State) ->
  {stop, normal, State};
handle_cast(Req, State) ->
  try
      do_handle_cast(Req, State)
  catch
      C:R:Stacktrace ->
        io:format("{{name}} handle_cast:~p fail:{~p,~p,~p}", [Req,C,R,Stacktrace]),
        {noreply, State}
  end.

%% @callback gen_server
handle_info(Info, State) ->
  try
      do_handle_info(Info, State)
  catch
      C:R:Stacktrace ->
        io:format("{{name}} handle_info:~p fail:{~p,~p,~p}", [Info,C,R,Stacktrace]),
        {noreply, State}
  end.

%% @callback gen_server
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% @callback gen_server
terminate(normal, _State) ->
  ok;
terminate(shutdown, _State) ->
  ok;
terminate({shutdown, _Reason}, _State) ->
  ok;
terminate(_Reason, _State) ->
  ok.

%%%.
%%%'   PRIVATE FUNCTIONS
-spec do_handle_call(Req::term(), From::{pid(),Tag}, State::term()) ->
   {reply,Reply::term(),NewState::term()} 
  | {reply,Reply::term(),NewState::term(),Timeout::int()>=0 | infinity}
  | {reply,Reply::term(),NewState::term(),hibernate}
  | {reply,Reply::term(),NewState::term(),{continue,Continue::term()}}
  | {noreply,NewState::term()} 
  | {noreply,NewState::term(),Timeout::int()>=0 | infinity}
  | {noreply,NewState::term(),hibernate}
  | {noreply,NewState::term(),{continue,Continue::term()}}
  | {stop,Reason::term(),Reply::term(),NewState::term()} | {stop,Reason::term(),NewState::term()}.
do_handle_call(_Req, _From, State) ->
  {noreply, State}.

-spec do_handle_cast(Req::term(), State::term()) ->
   {noreply,NewState::term()} 
  | {noreply,NewState::term(),Timeout::int()>=0 | infinity}
  | {noreply,NewState:term(),hibernate}
  | {noreply,NewState:term(),{continue,Continue::term()}}
  | {stop,Reason::normal | term(),NewState}.
do_handle_cast(_Req, State) ->
  {noreply, State}.

-spec do_handle_info(Info::timeout | term(), State::term()) ->
   {noreply,NewState::term()} 
  | {noreply,NewState::term(),Timeout::int()>=0 | infinity}
  | {noreply,NewState:term(),hibernate}
  | {noreply,NewState:term(),{continue,Continue::term()}}
  | {stop,Reason::normal | term(),NewState}.
do_handle_info(_Info, State) ->
  {noreply, State}.

%%%.
%%% vim: set filetype=erlang tabstop=2 foldmarker=%%%',%%%. foldmethod=marker:
