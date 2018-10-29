%%% @author {{author_name}} <{{author_email}}>
%%% @copyright {{copyright_year}} {{author_name}}
%%% @doc gen_event that {{description}}
%%% @end
-module({{name}}).
-author('{{author_name}} <{{author_email}}>').

-behaviour(gen_event).

-export([init/1, handle_event/2, terminate/2]).

-type arg()     :: term().
-type event()   :: term().
-type state()   :: term().
-type handler() :: module() | {module(), id()}.
-type id()      :: term().

%%====================================================================
%% API functions
%%====================================================================

%%====================================================================
%% Gen_event callbacks
%%====================================================================

-spec init([arg()]) -> {ok, [arg()]}.
%% @private
%% @doc initializes gen_event
init([]) ->
    {ok, []}.

-spec handle_event(event(), state()) ->
    {ok, state()} | {ok, state(), hibernate} |
    {swap_handler, arg(), state(), handler(), arg()} | remove_handler.
%% @private
%% @doc handle/log event
handle_event(_Message, State) ->
    {ok, State}.

-type terminate_args() :: term() | {stop, term()} | stop | remove_handler |
    {error, {'EXIT', term()}} | {error, term()}.
-spec terminate(terminate_args(), state()) -> ok.
%% terminates gen_event
terminate(_Args, _State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
