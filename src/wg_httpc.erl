%%%----------------------------------------------------------------------
%%%
%%% wg, Copyright (C) 2009 
%%%
%%% @author litao cheng <litaocheng@gmail.com>
%%% @doc http client base inet http client
%%%
%%%----------------------------------------------------------------------
-module(wg_httpc).
-author('litaocheng@gmail.com').
-vsn('0.1').
-include("wg.hrl").

-export([get/1, post/2, post/3]).
-define(TIMEOUT, 2000).

%% @doc http get request
-spec get(Url :: string()) ->
    {'ok', any()} | {'error', any()}.
get(Url) ->
    HttpOpts = [{timeout, ?TIMEOUT}],
    Opts = [{sync, true}, {body_format, binary}, {full_result, false}],
    http:request(get, {Url, []}, HttpOpts, Opts).

%% @doc http post request
-spec post(Url :: string(), Data :: iodata()) ->
    {'ok', any()} | {'error', any()}.
post(Url, Data) ->
    post(Url, "application/x-www-form-urlencoded", Data).

%% @doc http post request
-spec post(Url :: string(), Type :: string(), Data :: iodata()) ->
    {'ok', any()} | {'error', any()}.
post(Url, Type, Data) ->
    HttpOpts = [{timeout, ?TIMEOUT}],
    Opts = [{sync, true}, {body_format, binary}, {full_result, false}],
    http:request(post, {Url, [], Type, iolist_to_binary(Data)}, HttpOpts, Opts).
