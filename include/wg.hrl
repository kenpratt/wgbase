%%%----------------------------------------------------------------------
%%%
%%% @copyright litaocheng 2010
%%%
%%% @author litaocheng@gmail.com
%%% @doc crawl header file
%%%
%%%----------------------------------------------------------------------
-include("wg_log.hrl").

-include_lib("kernel/include/inet.hrl").

-ifdef(UNITTEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% define the user token 
-record(wg_token, {
        user = <<>> :: binary(),        % user name
        pwd = <<>> :: binary(),         % pwd 
        keystr = <<>> :: binary(),      %
        logint = 0 :: non_neg_integer() % login time
    }).
-type wg_token() :: #wg_token{}.

%% Print in standard output
-define(PRINT(Format, Args),
    io:format(Format, Args)).

%% syntax similar with ?: in c
-ifndef(IF).
-define(IF(C, T, F), (case (C) of true -> (T); false -> (F) end)).
-endif.
