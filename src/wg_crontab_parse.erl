%%%----------------------------------------------------------------------
%%%
%%% wg copyright © 2009
%%%
%%% @author litao cheng <litaocheng@gmail.com>
%%% @doc the crontab parse module (simlar with *unix crontab, please read
%%%      crontab.
%%%
%%%----------------------------------------------------------------------
-module(wg_crontab_parse).
-author('litaocheng@gmail.com').
-vsn('0.1').
-include("wg.hrl").
-include("wg_crontab.hrl").

-export([parse/1]).

%% @doc parse the crontab config file
-spec parse(File :: string()) ->
    {ok, [cron_entry()]} | {'error', any()}.
parse(File) ->
    case file:consult(File) of
        {error, enoent} = Error ->
            ?WARN2("crontab file ~p not exist", [File]),
            Error;
        {error, R} = Error ->
            ?WARN2("crontab file error: ~p", [file:format_error(R)]),
            Error;
        {ok, CronTab} ->
            parse_entrys(CronTab)
    end.

%%-----------------------------------------------------------------------
%%
%% internal API
%%
%%-----------------------------------------------------------------------

%% parse all the entrys
parse_entrys(CronTab) ->
    Entrys =
    lists:foldl(
        fun(Entry, Acc) ->
                case catch parse_entry(Entry) of
                    {ok, CronEntry} ->
                        [CronEntry | Acc];
                    {error, R} ->
                        ?WARN2("the line :~p error:~p", [Entry, R]),
                        Acc
                end
        end,
        [],
        CronTab),
    {ok, Entrys}.

%% parse the single entry
parse_entry({{M, H, Dom, Mon, Dow}, {Mod, F, A} = MFA}) when is_atom(Mod), is_atom(F), is_list(A) ->
    Cron =
    #cron_entry{
        m = parse_field(M, 0, 59, {error, emin}),
        h = parse_field(H, 0, 23, {error, ehour}),
        dom = parse_field(Dom, 1, 31, {error, edom}),
        mon = parse_field(Mon, 1, 12, {error, emon}),
        dow = parse_field(Dow, 0, 7, {error, edow}),
        mfa = MFA
    },
    {ok, Cron}; 
parse_entry(_) ->
    {error, eformat}.


%% parset the fileld
parse_field(F, Min, Max, Error) ->
    try parse_field(F, Min, Max) of
        Raw ->
            raw_to_record(Raw, Min, Max, Error)
    catch _:_ ->
        throw(Error)
    end.

parse_field(F, Min, Max) when is_list(F) ->
    case string:tokens(F, ",") of
        [Single] -> % is single value
            parse_value(Single, Min, Max);
        [_|_] = Multi -> % is list of values
            {?CRON_LIST, [parse_value(E, Min, Max) || E <- Multi]}
    end.

%% parse a value string: "*", "15", "*/2", "2-5/2", "2-5"
%%
%% "*"
parse_value("*", _Min, _Max) ->
    {?CRON_ANY};
%%
%% "15", "*/2", "2-5/2", "2-5"
parse_value(Str, Min, Max) when is_list(Str) ->
    case (string:chr(Str, $-) > 0) orelse (string:chr(Str, $/) > 0) of
        true ->
            parse_range(Str, Min, Max);
        false ->
            parse_num(Str, Min, Max)
    end.

%% parse a num string: "15"
parse_num(Str, Min, Max) ->
    case list_to_integer(Str) of
        N when N >= Min, N =< Max ->
            {?CRON_NUM, N};
        _ ->
            error
    end.

%% parse a range string: "*/2", "2-5/2", "2-5"
parse_range(Str, Min, Max) ->
    {RangeStr, Step} =
    case string:tokens(Str, "/") of
        [Range] ->
            {Range, 1};
        [Range, StepStr] ->
            {Range, list_to_integer(StepStr)}
    end,
    [First, Last] =
    case RangeStr of
        "*" ->
            [Min, Max];
        _ ->
            [list_to_integer(S) || S <- string:tokens(RangeStr, "-")]
    end,
    case {First, Last, Step} of
        {First, Last, Step} = RangeSpec when First >= Min, Last =< Max, Step >= Min, Step =< Max ->
            {?CRON_RANGE, RangeSpec};
        _ ->
            error
    end.

%% convert the intermediate format into records
raw_to_record(Raw, Min, Max, Error) ->
    case Raw of
        {?CRON_ANY} ->
            #cron_field{type = ?CRON_RANGE, value = {Min, Max, 1}};
        {?CRON_NUM, N} ->
            #cron_field{type = ?CRON_NUM, value = N};
        {?CRON_RANGE, {_First, _Last, _Step} = Range} ->
            #cron_field{type = ?CRON_RANGE, value = Range};
        {?CRON_LIST, List} ->
            #cron_field{type = ?CRON_LIST, value = [raw_to_record(I, Min, Max, Error) || I <- List]};
        _ ->
            throw(Error)
    end.
