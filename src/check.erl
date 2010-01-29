-module(check).
%-compile([export_all]).
-export([start_msglen_check/1, start_memory_check/1]).
-export([memory/0, msglen/0]).

-record(format, 
    {
        human_number,
        use_pidname
    }).

%% @doc return the minmax info of the specfy item in all the processes
%% example:
%% minmax_info([memory]) 
%% return : [{memory, {min, Min, Pid}, {max, Max, Pid}, {avg, Avg}}]
minmax_info(ItemSpec, #format{human_number=Human, use_pidname=PidName}) when is_list(ItemSpec) ->
    F = fun(P, Acc) ->
            try
                begin
                    R = 
                    erlang:process_info(P, ItemSpec),
                    {ItemSpec, ItemVal} = lists:unzip(R),
                    V = list_to_tuple([P | ItemVal]),
                    [V | Acc]
                end 
            catch
                _:_ ->
                    Acc
            end
        end,

    %% Infos : [{Pid, Item1, Item2, ...}]
    Infos = 
    lists:foldl(F, [], processes()),

    Len = length(Infos),

    Items = lists:zip(lists:seq(2, length(ItemSpec)+1), ItemSpec),
    FStatis = fun({N, Item}, Acc) ->
        Sort = lists:keysort(N, Infos),
        MinItem = hd(Sort),
        Min = element(N, MinItem),
        MinPid = element(1, MinItem),

        MaxItem = lists:nth(Len, Sort),
        Max = element(N, MaxItem),
        MaxPid = element(1, MaxItem),

        Avg = get_avg(N, Infos),
       
        Result = {Item, {min, to_human(Min, Human), name_or_pid(MinPid, PidName)}, 
                        {max, to_human(Max, Human), name_or_pid(MaxPid, PidName)}, 
                        {avg, Avg}},
        [Result | Acc]
    end,

    lists:reverse(lists:foldl(FStatis, [], Items)).


to_human(N, true) ->
    to_human(N);
to_human(N, false) ->
    N.

name_or_pid(Pid, true) when is_pid(Pid) ->
    name_or_pid(Pid);
name_or_pid(Pid, true) when is_pid(Pid) ->
    Pid.

%% @doc check all the processes, if any process extend the 
%% Max mesaage queue len threshold, then throw an error
start_msglen_check(Max) ->
    spawn(
        fun() ->
                check_loop(fun() -> check_msglen(Max) end)
    end).

%% @doc check all the processes memory
start_memory_check(Max) ->
    spawn(
        fun() ->
                check_loop(fun() -> check_memory(Max) end)
    end).


check_loop(Action) ->
    timer:sleep(2000),
    Action(),    
    check_loop(Action).

%% check if any process message len great than Max
check_msglen(Max) ->
    [Ret] =  minmax_info([message_queue_len], #format{human_number=false, use_pidname=true}),
    MaxInfo = element(3, Ret),
    Len = element(2, MaxInfo),
    if 
        Len > Max ->
            throw({error, {element(3, MaxInfo), "recach the max message queue len", Max}});
        true ->
            ok
    end.

check_memory(Max) ->
    io:format("max :~p~n", [Max]),
    [Ret] =  minmax_info([memory], #format{human_number=false, use_pidname=true}),
    
    io:format("ret:~p~n", [Ret]),
    MaxInfo = element(3, Ret),
    Len = element(2, MaxInfo),
    if 
        Len > Max ->
            throw({error, {element(3, MaxInfo), "recach the max memory:",  Max}});
        true ->
            ok
    end.

set_check_timer(Msg) ->
    erlang:start_timer(2000, self(), Msg).


%% @doc show memory info
memory() ->
    Ret = minmax_info([memory], #format{human_number=true, use_pidname=true}),

    io:format("memory:~n~p~n", [Ret]).

%% @doc message queue length
msglen() ->
    Ret = minmax_info([message_queue_len], #format{human_number=true, use_pidname=true}),
    io:format("msglen:~n~p~n", [Ret]).

%%
%% internal API
%%
get_sum(Index, List) ->
    lists:foldl(fun(E, Acc) ->
                Acc + element(Index, E)
        end,
        0, 
        List).


get_avg(Index, List) ->
    Sum = get_sum(Index, List),
    Sum / length(List).


name_or_pid(Pid) ->
    case erlang:process_info(Pid, registered_name) of
        {registered_name, Name} ->
            Name;
        [] ->
            Pid
    end.

units() ->
    {1024, "Byte", "KB", "MB", "GB"}.

to_human(0) ->
    0;
to_human(N) when N > 0 ->
    to_human1(N, {0, 1}).


to_human1(N, {R, Level}) when N < 1.0 ->
    Format = if is_integer(R) -> "~B ~s"; is_float(R) -> "~.2f ~s" end,
    lists:flatten(io_lib:format(Format, [R, element(Level, units())])); 
to_human1(N, {_R, Level}) when N > 0 ->
    %io:format("N:~p R:~p~n", [N, _R]),
    N2 = N / element(1, units()),
    to_human1(N2, {N, Level + 1}).

