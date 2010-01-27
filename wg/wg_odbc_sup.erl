%%%----------------------------------------------------------------------
%%%
%%% wg copyright © 2009
%%%
%%% @author litaocheng@gmail.com
%%% @doc the odbc module
%%%
%%%----------------------------------------------------------------------
-module(wg_odbc_sup).
-author('litaocheng@gmail.com').
-vsn('0.1').
-include("wg.hrl").


%% API
-export([start_link/0,
        init/1,
        get_pids/0,
        get_random_pid/0
    ]).

-define(DEFAULT_POOL_SIZE, 10).
-define(DEFAULT_ODBC_START_INTERVAL, 30). % 30 seconds
-define(SERVER, ?MODULE).

start_link() ->
    ?Debug2("start the wg_odbc_sup", []),
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).  

init(_Args) ->
    PoolSize = 
    case wg_config:get(?WG_CONF, poolsize) of
        I when is_integer(I) ->
           I;
        undefined ->
           ?DEFAULT_POOL_SIZE;
        Other ->
            ?Error2("Wrong odbc_pool_size definition '~p' for host ~p, default to ~p~n",
                [Other, ?DEFAULT_POOL_SIZE]),
            ?DEFAULT_POOL_SIZE
    end,

    StartInterval =
    case wg_config:get(?WG_CONF, odbc_start_interval) of
        Interval when is_integer(Interval) ->
            Interval;
        undefined ->
            ?DEFAULT_ODBC_START_INTERVAL;
        _Other2 ->
            ?Error2("Wrong odbc_start_interval definition '~p' "
                        ", defaulting to ~p~n",
            [_Other2, ?DEFAULT_ODBC_START_INTERVAL]),
            ?DEFAULT_ODBC_START_INTERVAL
    end,

    {ok, {{one_for_one, PoolSize+1, StartInterval},
        lists:map(
            fun(I) ->
                {I, {wg_odbc, start_link, [StartInterval*1000]},
                    transient, brutal_kill, worker, [?MODULE]}
            end, 
        lists:seq(1, PoolSize))}
    }.

get_pids() ->
    [Child || 
        {_Id, Child, _Type, _Modules} <- supervisor:which_children(?SERVER), Child /= undefined].

get_random_pid() ->
    Pids = get_pids(),
    lists:nth(erlang:phash(now(), length(Pids)), Pids).
