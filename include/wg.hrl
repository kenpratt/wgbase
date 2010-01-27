%%%----------------------------------------------------------------------
%%%
%%% wg, Copyright (C) 2009 
%%%
%%% @author litaocheng@gmail.com
%%% @doc the wg header file
%%%
%%%----------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").

%% Print in standard output
-define(PRINT(Format, Args),
    io:format(Format, Args)).

%% error macros
-define(Debug(F), 
    wg_logger:debug_msg(?MODULE, ?LINE, F, [])).
-define(Debug2(F, D), 
    wg_logger:debug_msg(?MODULE, ?LINE, F, D)).

-define(Info(F), 
    wg_logger:info_msg(?MODULE, ?LINE, F, [])).
-define(Info2(F, D),
    wg_logger:info_msg(?MODULE, ?LINE, F, D)).

-define(Warn(F),
    wg_logger:warning_msg(?MODULE, ?LINE, F, [])).
-define(Warn2(F, D), 
    wg_logger:warning_msg(?MODULE, ?LINE, F, D)).

-define(Error(F),
    wg_logger:error_msg(?MODULE, ?LINE, F, [])).
-define(Error2(F, D),
    wg_logger:error_msg(?MODULE, ?LINE, F, D)).

-define(Critical(F), 
    wg_logger:critical_msg(?MODULE, ?LINE, F, [])).
-define(Critical2(F, D), 
    wg_logger:critical_msg(?MODULE, ?LINE, F, D)).

%% the wg config
-define(WG_CONF, wg_config_tab).
-define(WG_CONF_FILE, "wg.cfg").

-define(WC_GET(Key), wg_config:get(?WG_CONF, Key)).
-define(WC_GET2(Key, Def), wg_config:get(?WG_CONF, Key, Def)).

%% define the user token 
-record(wg_token, {
        user = <<>> :: binary(),        % user name
        pwd = <<>> :: binary(),         % pwd 
        keystr = <<>> :: binary(),      %
        logint = 0 :: non_neg_integer() % login time
    }).
-type wg_token() :: #wg_token{}.


-define(NOT_IMPL, throw({not_impl, ?MODULE, ?LINE})).

%% some convert macros
-define(B2S(B), (if is_binary(B) -> binary_to_list(B) end)).
-define(S2B(S), (if is_list(S) -> list_to_binary(S) end)).

-define(N2S(N), (if is_integer(N) -> integer_to_list(N) end)).
