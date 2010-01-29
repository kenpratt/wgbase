%%%----------------------------------------------------------------------
%%%
%%% wg copyright © 2009 
%%%
%%% @author litaocheng@gmail.com
%%% @doc the timer manager
%%%
%%%----------------------------------------------------------------------
-module(wg_timer_mgr).
-author('litaocheng@gmail.com').
-vsn('0.1').
-behaviour(gen_server).

-include("wg.hrl").

-export([start_link/2]).
-export([start_timer/4, start_timer/5, start_timer/6, stop_timer/2]).
-export([get_remain/2]).

%% about the data operates
-export([get_userdata1/2, get_userdata2/2, get_userdatas/2]).
-export([update_userdata1/3, update_userdata2/3]).
-export([get_entry/2, delete_entry/2]).


-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
                            terminate/2, code_change/3]).

-define(UD_DEF, null).

%-type time() :: {integer(), integer(), integer()}.
-type time() :: non_neg_integer().

-record(timer_entry, {
        key :: any(),                   % key
        timer_ref :: reference(),       % the timer ref
        start = 0 :: time(),            % the timer start point (seconds)
        stop = 0 :: time(),             % the timer stop point (seconds)
        userdata1 = ?UD_DEF :: any(),   % first user defined data
        userdata2 = ?UD_DEF :: any()    % second user defined data
    }).
-type timer_entry() :: #timer_entry{}.

-record(state, {
        tab_name :: atom(),             % ets name
        callback :: atom()              % the callback module
    }).

%% @doc start the timer manager server
-spec start_link(Name :: atom(), Module :: atom()) -> any().
start_link(Name, Module) ->
    ?DEBUG2("start_link ~p callback module ~p ~n", [Name, Module]),
    gen_server:start_link({local, Name}, ?MODULE, {Name, Module}, []).

%% @doc start the timer, Time is in milliseconds
-spec start_timer(Name :: atom(), Time :: pos_integer(), 
    Msg :: any(), Key :: any()) -> 'ok'.
start_timer(Name, Time, Msg, Key) ->
    start_timer(Name, Time, Msg, Key, ?UD_DEF, ?UD_DEF).

-spec start_timer(Name :: atom(), Time :: pos_integer(), 
    Msg :: any(), Key :: any(), UserData1 :: any()) -> 'ok'.
start_timer(Name, Time, Msg, Key, UserData1) ->
    start_timer(Name, Time, Msg, Key, UserData1, ?UD_DEF).

-spec start_timer(Name :: atom(), Time :: pos_integer(), 
    Msg :: any(), Key :: any(), UserData1 :: any(), UserData2 :: any()) -> 'ok'.
start_timer(Name, Time, Msg, Key, UserData1, UserData2) ->
    ?DEBUG2("start timer time:~p msg:~p key:~p u1:~p u2:~p",
        [Time, Msg, Key, UserData1, UserData2]),
    TRef = erlang:start_timer(Time, whereis(Name), {Msg, Key}),
    Now = misc_util:time_ms(),
    Entry = #timer_entry{
        key = Key, 
        timer_ref = TRef,
        start = Now,
        stop = Now + Time,
        userdata1 = UserData1, 
        userdata2 = UserData2
    },
    true = ets:insert(Name, Entry),
    ok.
                    
%% @doc stop the timer
stop_timer(Name, Key) ->
    case ets:lookup(Name, Key) of
        [#timer_entry{timer_ref = TRef}] ->
            erlang:cancel_timer(TRef),
            true = ets:delete(Name, Key),
            ok;
        [] ->
            ?DEBUG2("the time not exist :~p", [Key]),
            ok
    end.

%% @doc get the remain timer in milliseconds
-spec get_remain(Name :: atom(), Key :: any()) ->
    non_neg_integer().
get_remain(Name, Key) ->
    case ets:lookup(Name, Key) of
        [#timer_entry{start = Start, stop = Stop}] -> 
            false = is_expire(Stop),
            Stop - Start;
        [] ->
            0 
    end.

%% @doc get the userdata1
-spec get_userdata1(Name :: atom(), Key :: any()) ->
    {'ok', any()} | 'none'.
get_userdata1(Name, Key) ->
    case ets:lookup(Name, Key) of
        [#timer_entry{userdata1 = U1}] -> 
            {ok, U1};
        [] ->
            none 
    end.

%% @doc get the userdata2
-spec get_userdata2(Name :: atom(), Key :: any()) ->
    {'ok', any()} | 'none'.
get_userdata2(Name, Key) ->
    case ets:lookup(Name, Key) of
        [#timer_entry{userdata2 = U2}] -> 
            {ok, U2};
        [] ->
            none 
    end.


%% @doc get the userdatas
-spec get_userdatas(Name :: atom(), Key :: any()) ->
    {any(), any()} | 'none'.
get_userdatas(Name, Key) -> 
    case ets:lookup(Name, Key) of
        [#timer_entry{userdata1 = U1, userdata2 = U2}] -> 
            {U1, U2};
        [] ->
            none 
    end.

%% @doc update the userdata1
-spec update_userdata1(Name :: atom(), Key :: any(), NewValue :: any()) ->
    boolean().
update_userdata1(Name, Key, NewValue) ->
    ets:update_element(Name, Key, {#timer_entry.userdata1, NewValue}).

%% @doc update the userdata2
-spec update_userdata2(Name :: atom(), Key :: any(), NewValue :: any()) ->
    boolean().
update_userdata2(Name, Key, NewValue) ->
    ets:update_element(Name, Key, {#timer_entry.userdata2, NewValue}).

%% @doc get the entry
-spec get_entry(Name :: atom(), Key :: any()) -> 
    {'ok', timer_entry()} | {'error', any()}.
get_entry(Name, Key) -> 
    case ets:lookup(Name, Key) of
        [#timer_entry{} = Entry] ->
            {ok, Entry};
        [] ->
            {error, not_exists}
    end.
            
%% @doc delete the entry
-spec delete_entry(Name :: atom(), Key :: any()) -> 'true'.
delete_entry(Name, Key) ->
    ets:delete(Name, Key).

%%
%% gen_server callbacks
%%
init({Name, Module}) ->
    process_flag(trap_exit, true),
    Name = ets:new(Name, [set, public, named_table, 
                            {keypos, #timer_entry.key}, 
                            {write_concurrency, true}]),
    ?DEBUG2("~p (callback ~p) init ...", [Name, Module]),
    {ok, #state{tab_name = Name, callback = Module}}.

handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({timeout, _TRef, {Msg, Key}}, State = #state{tab_name = Tab, callback = Mod}) ->
    Ret = (catch Mod:handle_timer(Tab, Msg, Key)),
    ?DEBUG2("handle_timer return :~p", [Ret]),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_Old, State, _Extra) ->
    {ok, State}.
    
%%
%% internal API
%%

%% is the Time expire?
is_expire(Time) ->
    misc_util:time_ms() > Time.
