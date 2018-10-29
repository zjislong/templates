#!/usr/bin/env escript
%% -*- mode: erlang -*-
%% @author    {{author_name}} <{{author_email}}>
%% @copyright {{copyright_year}} {{author_name}}
%% @doc       {{description}}
%% @end
-export([main/1]).
-define(CMD, filename:basename(escript:script_name())).
%%====================================================================
%% API functions
%%====================================================================
-spec main(Args) -> void()
  when
  Args :: list(string()).
main(_)->
  usage().

%%====================================================================
%% Internal functions
%%====================================================================
-spec usage() -> void().
usage() ->
  io:format("Usage: ~s ...~n", [?CMD]),
  halt(1).
