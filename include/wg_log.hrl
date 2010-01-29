%%%----------------------------------------------------------------------
%%%
%%% @copyright wg
%%%
%%% @author litaocheng@gmail.com
%%% @doc the log header
%%%
%%%----------------------------------------------------------------------
-ifndef(WG_LOG_HRL).
-define(WG_LOG_HRL, ok).

%% error macros
-ifdef(UNITTEST).

    -define(EUNITFMT(T, F, D),
        ?debugFmt(T ++ "(~p) " ++ F,
            [self()] ++ D)).

    -define(DEBUG(F), ?EUNITFMT("D", F, []).
    -define(DEBUG2(F, D), ?EUNITFMT("D", F, D)).

    -define(INFO(F), ?EUNITFMT("I", F, []).
    -define(INFO2(F, D), ?EUNITFMT("I", F, D)).

    -define(WARN(F), ?EUNITFMT("*W", F, []).
    -define(WARN2(F, D), ?EUNITFMT("*W", F, D)).

    -define(ERROR(F), ?EUNITFMT("**E**", F, []).
    -define(ERROR2(F, D), ?EUNITFMT("**E**", F, D)).

    -define(CRITICAL(F), ?EUNITFMT("C", F, []).
    -define(CRITICAL2(F, D), ?EUNITFMT("C", F, D)).

-else.
    -define(DEBUG(F), 
        wg_logger:debug_msg(?MODULE, ?LINE, F, [])).
    -define(DEBUG2(F, D),
        wg_logger:debug_msg(?MODULE, ?LINE, F, D)).

    -define(INFO(F), 
        wg_logger:info_msg(?MODULE, ?LINE, F, [])).
    -define(INFO2(F, D),
        wg_logger:info_msg(?MODULE, ?LINE, F, D)).

    -define(WARN(F),
        wg_logger:warning_msg(?MODULE, ?LINE, F, [])).
    -define(WARN2(F, D), 
        wg_logger:warning_msg(?MODULE, ?LINE, F, D)).

    -define(ERROR(F),
        wg_logger:error_msg(?MODULE, ?LINE, F, [])).
    -define(ERROR2(F, D),
        wg_logger:error_msg(?MODULE, ?LINE, F, D)).

    -define(CRITICAL(F), 
        wg_logger:critical_msg(?MODULE, ?LINE, F, [])).
    -define(CRITICAL2(F, D), 
        wg_logger:critical_msg(?MODULE, ?LINE, F, D)).
-endif.

-endif. % WG_LOG_HRL
