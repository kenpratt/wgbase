%%%----------------------------------------------------------------------
%%%
%%% wg, Copyright (C) 2009 
%%%
%%% @author litaocheng@gmail.com
%%% @doc the mnesia db util module
%%%
%%%----------------------------------------------------------------------
-module(wg_mnesia).
-export([init_db/0]).
-export([schema_type/0]).
-export([ensure_mnesia_not_running/0, ensure_mnesia_is_running/0]).

-include("wg.hrl").

%% @doc init the mensia db
-spec init_db() -> 'ok' | {'error', any()}.
init_db() ->
    ensure_mnesia_not_running(),
    case mnesia:create_schema([node()]) of
        {error, {_, {already_exists, _}}} ->
            ok = mnesia:start();
        ok -> %
            ok = mnesia:start()
    end.

%% @doc get the schema type on the node
-spec schema_type() -> {'error', 'not_exit'} | atom().
schema_type() ->
    case catch mnesia:table_info(schema, storage_type) of
        {'EXIT', {aborted, {no_exists, schema, _}}} ->
            {error, not_exit};
        Type ->
            Type
    end.

%% ensure mnesia not running
-spec ensure_mnesia_not_running() -> 'ok'.
ensure_mnesia_not_running() ->
    case mnesia:system_info(is_running) of
        no ->
            ok;
        _ ->
            throw({error, mnesia_unexpected_running})
    end.

%% ensure mnesia is running
-spec ensure_mnesia_is_running() -> 'ok'.
ensure_mnesia_is_running() ->
    case mnesia:system_info(is_running) of
        yes ->
            ok;
        _ ->
            throw({error, mnesia_not_running})
    end.

