%%%----------------------------------------------------------------------
%%%
%%% wg copyright © 2009
%%%
%%% @author litaocheng@gmail.com
%%% @doc the main entry of the node, wg will
%%%  first run the simp app, then run the logci app(tldh).
%%%  the wgctl start the apps from this module.
%%%
%%%----------------------------------------------------------------------
-module(wg).
-author('litaocheng@gmail.com').
-vsn('0.1').
-include("wg.hrl").

-export([start/0, stop/0]).

%% @doc start the system
-spec start() -> any().
start() ->
    ensure_apps(),
    wg_loglevel:set(5),

    wg_ctl:init(),

    ?Debug2("start the tldh app ~n", []),
    tldhapp:start(),

    ?Debug2("start the simp app ~n", []),
    simpapp:start().


%% @doc stop the system
-spec stop() -> 'ok'.
stop() ->
    ?Debug2("stop the simp app ~n", []),
    simpapp:stop(), 

    ?Debug2("stop the tldh app ~n", []),
    tldhapp:stop(),

    ?Debug2("stop the erlang vm ~n", []),
    init:stop().

%% start all the dependent applications
ensure_apps() ->
    application:start(sasl),
    inets:start(),
    ok.

