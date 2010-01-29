%%%----------------------------------------------------------------------
%%%
%%% wg, Copyright (C) 2009 
%%%
%%% @author litaocheng@gmail.com
%%% @doc manager the user session
%%%
%%%----------------------------------------------------------------------
-module(wg_session).
-author('litaocheng@gmail.com').
-vsn('0.1').
-include("wg.hrl").

-behaviour(gen_server).

-export([start_link/0, stop/0]).
-export([add/2, update/2, delete/1]).
-export([lookup/1, member/1]).
-export([reset/0]).
-export([count/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
                            terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(TAB, ?MODULE).

%% @doc start the session server
-spec start_link() -> any().
start_link() ->
    %?Debug2("start_link ~p ~n", [?SERVER]),
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc stop the session server
-spec stop() -> any().
stop() ->
    gen_server:call(?SERVER, stop).

%% @doc add an session, asume the key not exist
-spec add(Key :: any(), Val :: any()) -> boolean().
add(Key, Val) ->
    ets:insert_new(?TAB, {Key, Val}).
    
%% @doc update an sesion, the key must exist
-spec update(Key :: any(), Value :: any()) -> boolean().
update(Key, Val) ->
    ets:update_element(?TAB, Key, {2, Val}). 

%% @doc delete the specify key from table
-spec delete(Key :: any()) -> 'true'.
delete(Key) ->
    ets:delete_key(?TAB, Key). 

%% @doc lookup the object
-spec lookup(Key :: any()) -> {'value', tuple()} | 'none'.
lookup(Key) ->
    case ets:lookup(?TAB, Key) of
        [Obj] ->
            {value, Obj};
        [] ->
            none 
    end.

%% @doc lookup the object
-spec member(Key :: any()) -> boolean().
member(Key) ->
    ets:member(?TAB, Key).

%% @doc clear all the table content
-spec reset() -> 'true'.
reset() ->
    ets:delete_all_objects(?TAB).

%% @doc return the session list count
-spec count() -> non_neg_integer().
count() ->
    ets:info(?TAB, size).

%% gen_server callbacks
init([]) ->
    ets:new(?TAB, [set, protected, named_table, {keypos, 1}]),
    {ok, []}.

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_Old, State, _Extra) ->
    {ok, State}.
    
%%
%% internal API
%%

-ifdef(EUNIT).

session_test_() ->
    [
        {"session test",
         setup,
         fun() -> wg_session:start_link() end,
         fun(_) -> wg_session:stop() end,
         [
             ?_assertEqual(0, wg_session:count()),
             ?_assertEqual(none, wg_session:lookup(100))
         ]
        }
    ].
-endif.
